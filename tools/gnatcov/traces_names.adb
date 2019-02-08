------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

with GNAT.Strings;  use GNAT.Strings;

with Checkpoints;
with Coverage.Object;   use Coverage.Object;
with Coverage.Tags;     use Coverage.Tags;
with Files_Table;
with Inputs;            use Inputs;
with Outputs;           use Outputs;
with Switches;          use Switches;
with Symbols;           use Symbols;

package body Traces_Names is

   function "<" (Key1, Key2 : Subprogram_Key) return Boolean;
   function Equal (L, R : Subprogram_Info) return Boolean;

   function Format_CU
     (CU_Filename, CU_Directory : String_Access) return Symbol;
   --  Format a Compilation Unit symbol suitable for Subprogram_Key

   package Routine_Name_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Symbol,
      "<"          => "<",
      "="          => "=");

   package Routine_Tag_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_SC_Tag,
      Element_Type => Cst_String_Access);

   type Routine_Tag_Provider_Type is new Tag_Provider_Type with record
      Routine_Tags : Routine_Tag_Vectors.Vector;
   end record;

   overriding function Get_Slocs_And_Tags
     (TP : access Routine_Tag_Provider_Type;
      PC : Pc_Type) return Tagged_Slocs;

   overriding function Tag_Name
     (TP  : access Routine_Tag_Provider_Type;
      Tag : SC_Tag) return String;

   overriding function Map_Tag
     (TP     : access Routine_Tag_Provider_Type;
      CLS    : Checkpoints.Checkpoint_Load_State;
      CP_Tag : SC_Tag) return SC_Tag;

   package R is new Tag_Providers.Register_Factory
     (Name => "routine", T => Routine_Tag_Provider_Type);
   pragma Unreferenced (R);

   package Routines_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Subprogram_Key,
      Element_Type => Subprogram_Info,
      "<"          => "<",
      "="          => Equal);

   procedure Match_Routine_Insns
     (Exec      : Exe_File_Acc;
      Section   : Section_Index;
      Content   : Binary_Content;
      Subp_Info : in out Subprogram_Info;
      Success   : out Boolean);
   --  Match the machine instructions in Subp_Info.Insn and the (Exec, Section,
   --  Content) tuple: set Success to whether they indeed match.
   --
   --  In the case where they have inconsistent padding instruction, this tries
   --  to determine which instructions are padding and does the matching
   --  without considering these.  In this case, the Insn.Last and
   --  Padding_Stripped fields of Subp_Info are updated.

   Routines : Routines_Maps.Map;
   --  Each item stores coverage information for one (consolidated) routine.

   Routines_Of_Interest : Routine_Name_Sets.Set;
   --  Set of routine names that will be suject to coverage. It is only used to
   --  filter the creation of Subprogram_Info entries when processing
   --  traces/ELF files.

   Next_Origin : Natural := 1;
   --  Counter for Subprogram_Key.Origin

   -----------------
   -- Key_To_Name --
   -----------------

   function Key_To_Name (Key : Subprogram_Key) return Cst_String_Access
   is
   begin
      return To_String (Key.Name);
   end Key_To_Name;

   -----------------------------
   -- Add_Routine_Of_Interest --
   -----------------------------

   procedure Add_Routine_Of_Interest (Name : String) is
   begin
      Routines_Of_Interest.Insert (To_Symbol (Name));
   end Add_Routine_Of_Interest;

   ----------------------------
   -- Is_Routine_Of_Interest --
   ----------------------------

   function Is_Routine_Of_Interest (Name : String) return Boolean is
      use Routine_Name_Sets;
      Name_Symbol : constant Symbol := To_Symbol (Name);
   begin
      return Routines_Of_Interest.Find (Name_Symbol) /= No_Element;
   end Is_Routine_Of_Interest;

   --------------------------------
   -- Remove_Routine_Of_Interest --
   --------------------------------

   procedure Remove_Routine_Of_Interest (Name : String) is
      Name_Symbol : constant Symbol := To_Symbol (Name);
   begin
      Routines_Of_Interest.Exclude (Name_Symbol);
   end Remove_Routine_Of_Interest;

   ---------------
   -- Format_CU --
   ---------------

   function Format_CU
     (CU_Filename, CU_Directory : String_Access) return Symbol is
   begin
      if CU_Filename = null then
         return No_Symbol;
      elsif CU_Directory = null
         or else Files_Table.Is_Absolute_Path (CU_Filename.all)
      then
         return To_Symbol (CU_Filename.all);
      else
         return To_Symbol (CU_Directory.all & "/" & CU_Filename.all);
      end if;
   end Format_CU;

   -----------------
   -- Add_Routine --
   -----------------

   procedure Add_Routine
     (Key     : in out Subprogram_Key;
      Exec    : Exe_File_Acc;
      Section : Section_Index)
   is
      use Routines_Maps;
      TP  : Tag_Provider_Access renames Tag_Provider;
      Cur : Cursor;
      Tag : SC_Tag;

   begin
      --  If the routine has no compile unit, it must not be consolidated, so
      --  it is made unique using its Origin member.

      if Key.Compile_Unit = No_Symbol then
         Key.Origin := Next_Origin;
         Next_Origin := Next_Origin + 1;
      else
         Key.Origin := 0;
      end if;

      --  Insert a new subprogram info entry if there is no such one for the
      --  given key. Such a subprogram already exist when we are consolidating
      --  a symbol.

      Cur := Routines.Find (Key);
      if Cur = No_Element then

         --  If doing routine-based separated coverage analysis, record name in
         --  routine tags table.

         if TP.all in Routine_Tag_Provider_Type'Class then
            declare
               RTags : Routine_Tag_Vectors.Vector
                  renames Routine_Tag_Provider_Type (TP.all).Routine_Tags;
            begin
               RTags.Append (Key_To_Name (Key));
               Tag := RTags.Last_Index;
            end;
         else
            Tag := No_SC_Tag;
         end if;

         Routines.Insert
           (Key,
            Subprogram_Info'(Exec             => Exec,
                             Section          => Section,
                             Padding_Stripped => <>,
                             Insns            => Invalid_Binary_Content,
                             Traces           => null,
                             Offset           => 0,
                             Routine_Tag      => Tag));
      else
         --  If doing routine-based separated coverage analysis, take the tag
         --  of the consolidated subprogram.

         Tag := Element (Cur).Routine_Tag;
      end if;

      if Verbose and then Tag /= No_SC_Tag then
         Put_Line ("Routine tag" & Tag'Img & ": "  & Key_To_Name (Key).all);
      end if;
   end Add_Routine;

   -------------------
   -- Get_Subp_Info --
   -------------------

   function Get_Subp_Info (Key : Subprogram_Key) return Subprogram_Info is
   begin
      return Routines_Maps.Element (Routines.Find (Key));
   end Get_Subp_Info;

   ---------------------
   -- Key_From_Symbol --
   ---------------------

   procedure Key_From_Symbol
     (Exec : Exe_File_Acc;
      Sym  : Address_Info_Acc;
      Key  : out Subprogram_Key)
   is
      CU_Filename, CU_Directory : String_Access;
   begin
      Get_Compile_Unit (Exec.all, Sym.First, CU_Filename, CU_Directory);
      Key :=
        (Name         => To_Symbol (Sym.Symbol_Name.all),
         Compile_Unit => Format_CU (CU_Filename, CU_Directory),
         Origin       => Sym.Symbol_Origin);
   end Key_From_Symbol;

   -----------
   -- Is_In --
   -----------

   function Is_In (Key : Subprogram_Key) return Boolean is
      use Routines_Maps;
   begin
      return Routines.Find (Key) /= No_Element;
   end Is_In;

   --------------
   -- Add_Code --
   --------------

   procedure Add_Code
     (Subp_Key     : Subprogram_Key;
      Exec         : Exe_File_Acc;
      Section      : Section_Index;
      Content      : Binary_Content;
      First_Code   : out Boolean;
      Subp_Info    : out Subprogram_Info)
   is
      use Routines_Maps;
      use Interfaces;

      procedure Update
        (Subp_Key  : Subprogram_Key;
         Subp_Info : in out Subprogram_Info);
      --  Update the subprogram info of the routine identified by Key in the
      --  name table.

      ------------
      -- Update --
      ------------

      procedure Update
        (Subp_Key  : Subprogram_Key;
         Subp_Info : in out Subprogram_Info)
      is
         Success : Boolean;
      begin
         --  First, check if a trace base has already been added to the
         --  subprogram info; if so, check that it does not conflict with the
         --  one given in parameter.

         if not Is_Loaded (Subp_Info.Insns) and then Length (Content) > 0 then
            --  Subp_Info.Insns := new Binary_Content'(Content);
            --  TODO??? Is a copy really needed?
            Subp_Info.Insns := Content;
            Subp_Info.Exec := Exec;
            Subp_Info.Section := Section;
            Subp_Info.Offset := 0;
            First_Code := True;

         else
            --  Check that the Content passed in parameter is the same as the
            --  one we already registered; if the two contents were different
            --  (e.g. came from two executables compiled with different
            --  compilation options), the consolidation would not make sense.

            Match_Routine_Insns (Exec, Section, Content, Subp_Info, Success);
            if not Success then
               Put_Line (Standard_Error,
                         "error: different function size for "
                           & Key_To_Name (Subp_Key).all);
               Put_Line (Standard_Error,
                         " (reference is " & Get_Filename (Subp_Info.Exec.all)
                           & ", file is " & Get_Filename (Exec.all) & ")");
               raise Consolidation_Error;
            end if;

            Subp_Info.Offset := Subp_Info.Insns.First - Content.First;
         end if;

         Add_Code.Subp_Info := Subp_Info;
      end Update;

      Cur : constant Cursor := Routines.Find (Subp_Key);

   --  Start of processing for Add_Code

   begin
      First_Code := False;
      if Has_Element (Cur) then
         Routines.Update_Element (Cur, Update'Access);
      end if;
   end Add_Code;

   -------------------------
   -- Add_Code_And_Traces --
   -------------------------

   procedure Add_Code_And_Traces
     (Subp_Key     : Subprogram_Key;
      Exec         : Exe_File_Acc;
      Section      : Section_Index;
      Content      : Binary_Content;
      Base         : access Traces_Base)
   is
      use Routines_Maps;
      use Interfaces;

      procedure Update
        (Subp_Key  : Subprogram_Key;
         Subp_Info : in out Subprogram_Info);
      --  Update the subprogram info of the routine identified by Key in the
      --  name table.

      ------------
      -- Update --
      ------------

      procedure Update
        (Subp_Key  : Subprogram_Key;
         Subp_Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Subp_Key);
         Trace_Cursor : Entry_Iterator;
         Trace        : Trace_Entry;

         First, Last : Pc_Type;
      begin
         --  If these are the first traces we are loading, initialize the
         --  routine trace base.

         if Subp_Info.Traces = null then
            Subp_Info.Traces := new Traces_Base;
            Init_Base (Subp_Info.Traces.all);
         end if;

         --  Now, update the subprogram traces with the trace base given in
         --  parameter. Rebase the traces' addresses to the subprogram address
         --  range (i.e. Subp_Info.Insns'Range).

         Init (Base.all, Trace_Cursor, Content.First);
         Get_Next_Trace (Trace, Trace_Cursor);

         while Trace /= Bad_Trace loop
            exit when Trace.First > Content.Last;

            --  Note, trace may span several routines

            if Trace.Last >= Content.First then

               --  Ceil

               if Trace.First >= Content.First then
                  First := Trace.First + Subp_Info.Offset;
               else
                  First := Subp_Info.Insns.First;
               end if;

               --  Floor

               if Trace.Last <= Content.Last then
                  Last := Trace.Last + Subp_Info.Offset;
               else
                  Last := Subp_Info.Insns.Last;
               end if;

               --  Consistency check
               --  Shouldn't that be an Assert???

               if First > Last then
                  raise Program_Error;
               end if;

               Add_Entry (Subp_Info.Traces.all, First, Last, Trace.Op);
            end if;

            Get_Next_Trace (Trace, Trace_Cursor);
         end loop;
      end Update;

      Cur : Cursor;

      First_Code : Boolean;
      Subp_Info  : Subprogram_Info;
      pragma Unreferenced (First_Code, Subp_Info);

   --  Start of processing for Add_Code_And_Traces

   begin
      Add_Code (Subp_Key, Exec, Section, Content, First_Code, Subp_Info);
      Cur := Routines.Find (Subp_Key);
      if Has_Element (Cur) then
         Routines.Update_Element (Cur, Update'Access);
      end if;
   end Add_Code_And_Traces;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Proc    : access procedure (Subp_Key  : Subprogram_Key;
                                  Subp_Info : in out Subprogram_Info);
      Sorted  : Boolean := False)
   is
   begin
      if Sorted then
         declare
            type Subp_Entry is record
               Name   : Symbol;
               Cursor : Routines_Maps.Cursor;
            end record;

            function "<" (Left, Right : Subp_Entry) return Boolean is
              (Get (Left.Name).all < Get (Right.Name).all);

            package Subp_Vectors is new Ada.Containers.Vectors
              (Index_Type   => Positive,
               Element_Type => Subp_Entry);
            package Subp_Sorting is new Subp_Vectors.Generic_Sorting;

            Subps : Subp_Vectors.Vector;
            --  Sorted list for iteration

         begin
            Subps.Reserve_Capacity (Routines.Length);
            for Cur in Routines.Iterate loop
               Subps.Append ((Name   => Routines_Maps.Key (Cur).Name,
                              Cursor => Cur));
            end loop;
            Subp_Sorting.Sort (Subps);

            for Subp_Entry of Subps loop
               Routines.Update_Element (Subp_Entry.Cursor, Proc);
            end loop;
         end;

      else
         for Cur in Routines.Iterate loop
            Routines.Update_Element (Cur, Proc);
         end loop;
      end if;
   end Iterate;

   ---------------------------
   -- Compute_Routine_State --
   ---------------------------

   function Compute_Routine_State
     (Insns  : Binary_Content;
      Traces : Traces_Base_Acc) return Line_State
   is
      use type Pc_Type;

      State : Line_State := No_Code;
      Addr  : Pc_Type;
      It    : Entry_Iterator;
      T     : Trace_Entry;
   begin
      if not Is_Loaded (Insns) then

         --  The routine was not found in the executable

         return Not_Covered;

      else
         Init (Traces.all, It, 0);

         Addr := Insns.First;

         loop
            Get_Next_Trace (T, It);
            exit when T = Bad_Trace;
            if T.First > Addr then
               Update_Line_State (State, Insn_State'(Not_Covered));
               exit;
            end if;
            Update_Line_State (State, T.State);
            Addr := T.Last + 1;
         end loop;

         if Addr < Insns.Last then
            Update_Line_State (State, Insn_State'(Not_Covered));
         end if;

         if State = No_Code then
            return Not_Covered;
         else
            return State;
         end if;
      end if;
   end Compute_Routine_State;

   -----------------------
   -- Disp_All_Routines --
   -----------------------

   procedure Disp_All_Routines
   is
      use Routines_Maps;
      Cur : Cursor;
   begin
      Cur := Routines.First;
      while Has_Element (Cur) loop
         Put_Line (Key_To_Name (Key (Cur)).all);
         Next (Cur);
      end loop;
   end Disp_All_Routines;

   -----------------------------------
   -- Disp_All_Routines_Of_Interest --
   -----------------------------------

   procedure Disp_All_Routines_Of_Interest is
      use Routine_Name_Sets;
      Cur : Cursor;
   begin
      Cur := Routines_Of_Interest.First;
      while Has_Element (Cur) loop
         Put_Line (Get (Element (Cur)).all);
         Next (Cur);
      end loop;
   end Disp_All_Routines_Of_Interest;

   ---------
   -- "<" --
   -- ------

   function "<" (Key1, Key2 : Subprogram_Key) return Boolean
   is
   begin
      if Key1.Name < Key2.Name then
         return True;

      elsif Key1.Name = Key2.Name then
         if Key1.Compile_Unit < Key2.Compile_Unit then
            return True;
         elsif Key1.Compile_Unit = Key2.Compile_Unit then
            return Key1.Origin < Key2.Origin;
         else
            return False;
         end if;

      else
         return False;
      end if;
   end "<";

   -----------
   -- Equal --
   -----------

   function Equal (L, R : Subprogram_Info) return Boolean
   is
      pragma Unreferenced (L, R);
   begin
      return False;
   end Equal;

   ------------------------
   -- Get_Slocs_And_Tags --
   ------------------------

   overriding function Get_Slocs_And_Tags
     (TP  : access Routine_Tag_Provider_Type;
      PC  : Pc_Type) return Tagged_Slocs
   is
      use type Pc_Type;
   begin
      pragma Assert
        (PC in TP.Current_Routine.Insns.First + TP.Current_Routine.Offset
            .. TP.Current_Routine.Insns.Last  + TP.Current_Routine.Offset);
      return Get_Slocs_With_Tag
        (TP.Current_Subp.Lines, PC, TP.Current_Routine.Routine_Tag);
   end Get_Slocs_And_Tags;

   --------------
   -- Tag_Name --
   --------------

   overriding function Tag_Name
     (TP  : access Routine_Tag_Provider_Type;
      Tag : SC_Tag) return String
   is
   begin
      return TP.Routine_Tags.Element (Tag).all;
   end Tag_Name;

   -------------
   -- Map_Tag --
   -------------

   overriding function Map_Tag
     (TP     : access Routine_Tag_Provider_Type;
      CLS    : Checkpoints.Checkpoint_Load_State;
      CP_Tag : SC_Tag) return SC_Tag
   is
      pragma Unreferenced (TP, CLS, CP_Tag);
   begin
      Fatal_Error ("cannot perform incremental coverage with "
                   & "routines-based separation");
      return No_SC_Tag;
   end Map_Tag;

   ----------------------------------
   -- Read_Routine_Names_From_Text --
   ----------------------------------

   procedure Read_Routine_Names_From_Text (Filename : String) is
   begin
      Read_List_From_File (Filename, Add_Routine_Of_Interest'Access);
   exception
      when Name_Error | Status_Error =>
         Fatal_Error ("cannot open routine list: " & Filename);
   end Read_Routine_Names_From_Text;

   -------------------------
   -- Match_Routine_Insns --
   -------------------------

   procedure Match_Routine_Insns
     (Exec      : Exe_File_Acc;
      Section   : Section_Index;
      Content   : Binary_Content;
      Subp_Info : in out Subprogram_Info;
      Success   : out Boolean)
   is
      use type Pc_Type;

      Ref_Padding_First, New_Padding_First : Pc_Type;

   --  Start of processing for Match_Routine_Insns

   begin
      --  ??? Checking the actual content is actually quite complicated; we
      --  cannot just compare the binary content, as between two different
      --  executables the same symbol may be located in a different location.
      --  So far, we just make sure that the function has the same size in the
      --  two executables.
      --
      --  For some executables such as Windows', the size of routines is an
      --  approximation that includes padding instructions.  See how long the
      --  input routines really are when stripping trailing padding
      --  instructions.

      Ref_Padding_First :=
        (if Subp_Info.Padding_Stripped
         then Subp_Info.Insns.Last + 1
         else Find_Padding_First (Subp_Info.Exec,
                                  Subp_Info.Section,
                                  Subp_Info.Insns));
      New_Padding_First := Find_Padding_First (Exec, Section, Content);

      declare
         Ref_Padding_Offset : constant Pc_Type :=
            Ref_Padding_First - Subp_Info.Insns.First;
         New_Padding_Offset : constant Pc_Type :=
            New_Padding_First - Content.First;
      begin
         if Ref_Padding_Offset = New_Padding_Offset then
            Subp_Info.Insns.Last := Ref_Padding_First - 1;
            Subp_Info.Padding_Stripped := True;
            Success := True;
         else
            Success := False;
         end if;
      end;

   end Match_Routine_Insns;

end Traces_Names;
