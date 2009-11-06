------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2009, AdaCore                    --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces;

with Coverage;        use Coverage;
with Coverage.Object; use Coverage.Object;
with Inputs;          use Inputs;
with Outputs;         use Outputs;
with Strings;         use Strings;

package body Traces_Names is

   function Equal (L, R : Subprogram_Info) return Boolean;
   --  Needs comment???

   function Equal (L, R : Subprogram_Info) return Boolean
   is
      pragma Unreferenced (L, R);
   begin
      return False;
   end Equal;

   package Names_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => String_Access,
      Element_Type => Subprogram_Info,
      "<"          => "<",
      "="          => Equal);

   Names : Names_Maps.Map;
   --  Needs comments???
   --  Needs to be available to clients of this unit???

   --------------
   -- Add_Code --
   --------------

   procedure Add_Code
     (Routine_Name : String_Access;
      Exec         : Exe_File_Acc;
      Content      : Binary_Content;
      First_Code   : out Boolean)
   is
      use Names_Maps;
      use Interfaces;

      procedure Update
        (Subp_Name : String_Access;
         Subp_Info : in out Subprogram_Info);
      --  Update the subprogram info of the routine whose name is Key
      --  in the name table

      ------------
      -- Update --
      ------------

      procedure Update
        (Subp_Name : String_Access;
         Subp_Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Subp_Name);
      begin
         --  First, check if a trace base has already been added to the
         --  subprogram info; if so, check that it does not conflict with the
         --  one given in parameter.

         if Subp_Info.Insns = null and then Content'Length > 0 then
            Subp_Info.Insns := new Binary_Content'(Content);
            Subp_Info.Exec := Exec;
            Subp_Info.Offset := 0;
            First_Code := True;

         else
            --  Check that the Content passed in parameter is the same as the
            --  one we already registered; if the two contents were different
            --  (e.g. came from two executables compiled with different
            --  compilation options), the consolidation would not make sense.
            --  ??? Checking the actual content is actually quite complicated;
            --  we cannot just compare the binary content, as between two
            --  different executables the same symbol may be located in a
            --  different location. So far, we just make sure that the function
            --  has the same size in the two executables.

            if Content'Length /= Subp_Info.Insns.all'Length then
               Put_Line (Standard_Error,
                         "error: different function size for "
                           & Routine_Name.all);
               Put_Line (Standard_Error,
                         " (reference is " & Get_Filename (Subp_Info.Exec.all)
                           & ", file is " & Get_Filename (Exec.all) & ")");
               raise Consolidation_Error;
            end if;

            Subp_Info.Offset := Subp_Info.Insns'First - Content'First;
         end if;
      end Update;

      Cur : constant Cursor := Names.Find (Routine_Name);

   --  Start of processing for Add_Code

   begin
      First_Code := False;
      if Has_Element (Cur) then
         Names.Update_Element (Cur, Update'Access);
      end if;
   end Add_Code;

   -------------------------
   -- Add_Code_And_Traces --
   -------------------------

   procedure Add_Code_And_Traces
     (Routine_Name : String_Access;
      Exec         : Exe_File_Acc;
      Content      : Binary_Content;
      Base         : access Traces_Base)
   is
      use Names_Maps;
      use Interfaces;

      procedure Update
        (Subp_Name : String_Access;
         Subp_Info : in out Subprogram_Info);
      --  Update the subprogram info of the routine whose name is Key
      --  in the name table

      ------------
      -- Update --
      ------------

      procedure Update
        (Subp_Name : String_Access;
         Subp_Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Subp_Name);
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

         Init (Base.all, Trace_Cursor, Content'First);
         Get_Next_Trace (Trace, Trace_Cursor);

         while Trace /= Bad_Trace loop
            exit when Trace.First > Content'Last;

            if Trace.Last in Content'Range then
               if Trace.First >= Content'First then
                  First := Trace.First + Subp_Info.Offset;
               else
                  First := Subp_Info.Insns'First;
               end if;

               if Trace.Last <= Content'Last then
                  Last := Trace.Last + Subp_Info.Offset;
               else
                  Last := Subp_Info.Insns'Last;
               end if;

               Add_Entry (Subp_Info.Traces.all, First, Last, Trace.Op);
            end if;
            Get_Next_Trace (Trace, Trace_Cursor);
         end loop;
      end Update;

      Cur : Cursor;

      First_Code : Boolean;
      pragma Unreferenced (First_Code);

   --  Start of processing for Add_Code_And_Traces

   begin
      Add_Code (Routine_Name, Exec, Content, First_Code);
      Cur := Names.Find (Routine_Name);
      if Has_Element (Cur) then
         Names.Update_Element (Cur, Update'Access);
      end if;
   end Add_Code_And_Traces;

   ----------------------
   -- Add_Routine_Name --
   ----------------------

   procedure Add_Routine_Name
     (Name : String_Access;
      Exec : Exe_File_Acc := null)
   is
   begin
      Names.Insert (Name,
        Subprogram_Info'(Exec   => Exec,
                         Insns  => null,
                         Traces => null,
                         Offset => 0));
   end Add_Routine_Name;

   procedure Add_Routine_Name (Name : String) is
      Element : constant String_Access := new String'(Name);
      Cur     : constant Names_Maps.Cursor := Names.Find (Element);
   begin
      if Names_Maps.Has_Element (Cur) then
         Error ("symbol " & Name & " is already defined");
      else
         Add_Routine_Name (Element, null);
      end if;
   end Add_Routine_Name;

   ---------------------------
   -- Compute_Routine_State --
   ---------------------------

   function Compute_Routine_State
     (Insns  : Binary_Content_Acc;
      Traces : Traces_Base_Acc) return Line_State
   is
      use type Interfaces.Unsigned_32;
      State : Line_State := No_Code;
      Addr  : Pc_Type;
      It    : Entry_Iterator;
      T     : Trace_Entry;
   begin
      if Insns = null then
         --  The routine was not found in the executable

         return Not_Covered;

      else
         Init (Traces.all, It, 0);

         Addr := Insns'First;

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

         if Addr < Insns'Last then
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
      use Names_Maps;
      Cur : Cursor;
   begin
      Cur := Names.First;
      while Has_Element (Cur) loop
         Put_Line (Key (Cur).all);
         Next (Cur);
      end loop;
   end Disp_All_Routines;

   -------------------
   -- Get_Subp_Info --
   -------------------

   function Get_Subp_Info (Name : String_Access) return Subprogram_Info is
   begin
      return Names_Maps.Element (Names.Find (Name));
   end Get_Subp_Info;

   -----------
   -- Is_In --
   -----------

   function Is_In (Name : String_Access) return Boolean is
      use Names_Maps;
   begin
      return Names.Find (Name) /= No_Element;
   end Is_In;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Proc : access procedure (Subp_Name : String_Access;
                               Subp_Info : in out Subprogram_Info))
   is
      use Names_Maps;

      procedure Process_One (Cur : Cursor);
      --  Call Proc for the element at Cur

      procedure Process_One (Cur : Cursor) is
      begin
         Names.Update_Element (Cur, Proc);
      end Process_One;

   --  Start of processing for Iterate

   begin
      Names.Iterate (Process_One'Access);
   end Iterate;

   ----------------------------------
   -- Read_Routines_Name_From_Text --
   ----------------------------------

   procedure Read_Routines_Name_From_Text (Filename : String) is
   begin
      Read_List_From_File (Filename, Add_Routine_Name'Access);
   exception
      when Name_Error | Status_Error =>
         Fatal_Error ("cannot open routine list: " & Filename);
   end Read_Routines_Name_From_Text;

   -------------------------
   -- Remove_Routine_Name --
   -------------------------

   procedure Remove_Routine_Name (Name : String_Access) is
   begin
      Names.Exclude (Name);
   end Remove_Routine_Name;

end Traces_Names;
