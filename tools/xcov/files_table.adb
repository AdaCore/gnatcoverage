------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;
with Traces_Sources;

package body Files_Table is

   package File_Dynamic_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => File_Info_Access,
      Table_Index_Type     => Source_File_Index,
      Table_Low_Bound      => First_Source_File,
      Table_Initial        => 16,
      Table_Increment      => 100);

   File_Table : File_Dynamic_Tables.Instance;

   procedure Expand_Line_Table (File : Source_File_Index; Line : Natural);
   --  If Line is not in File's line table, expand this table and mark the new
   --  line as No_Code.

   procedure Append
     (Info            : Line_Info_Access;
      State           : Line_State;
      Instruction_Set : Addresses_Info_Acc;
      Base            : Traces_Base_Acc;
      Exec            : Exe_File_Acc);
   --  Comment needed???

   Filenames : Filename_Vectors.Vector;
   package Filename_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => String_Acc,
      Element_Type    => Source_File_Index,
      Hash            => Hash,
      Equivalent_Keys => Equal,
      "="             => "=");

   Filename_Map : Filename_Maps.Map;

   --  Source rebase/search types

   type Source_Search_Entry;
   type Source_Search_Entry_Acc is access Source_Search_Entry;
   type Source_Search_Entry is record
      Prefix : String_Acc;
      Next : Source_Search_Entry_Acc;
   end record;

   type Source_Rebase_Entry;
   type Source_Rebase_Entry_Acc is access Source_Rebase_Entry;
   type Source_Rebase_Entry is record
      Old_Prefix : String_Acc;
      New_Prefix : String_Acc;
      Next : Source_Rebase_Entry_Acc;
   end record;

   First_Source_Rebase_Entry : Source_Rebase_Entry_Acc := null;
   Last_Source_Rebase_Entry  : Source_Rebase_Entry_Acc := null;

   First_Source_Search_Entry : Source_Search_Entry_Acc := null;
   Last_Source_Search_Entry  : Source_Search_Entry_Acc := null;

   -----------------------
   -- Add_Source_Search --
   -----------------------

   procedure Add_Source_Search (Prefix : String)
   is
      E : Source_Search_Entry_Acc;
   begin
      E := new Source_Search_Entry'(Prefix => new String'(Prefix),
                                    Next => null);
      if First_Source_Search_Entry = null then
         First_Source_Search_Entry := E;
      else
         Last_Source_Search_Entry.Next := E;
      end if;
      Last_Source_Search_Entry := E;
   end Add_Source_Search;

   -----------------------
   -- Add_Source_Rebase --
   -----------------------

   procedure Add_Source_Rebase (Old_Prefix : String; New_Prefix : String) is
      E : Source_Rebase_Entry_Acc;
   begin
      E := new Source_Rebase_Entry'(Old_Prefix => new String'(Old_Prefix),
                                    New_Prefix => new String'(New_Prefix),
                                    Next => null);
      if First_Source_Rebase_Entry = null then
         First_Source_Rebase_Entry := E;
      else
         Last_Source_Rebase_Entry.Next := E;
      end if;
      Last_Source_Rebase_Entry := E;
   end Add_Source_Rebase;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Name : String) return Source_File_Index is
      use Filename_Maps;
      Nam : aliased String := Name;
      Cur : constant Cursor := Filename_Map.Find (Nam'Unrestricted_Access);
   begin
      if Cur /= No_Element then
         return Element (Cur);

      else
         declare
            New_Name : constant String_Acc := new String'(Name);
         begin
            Filenames.Append (New_Name);
            Filename_Map.Insert (New_Name, Filenames.Last_Index);
            return Filenames.Last_Index;
         end;
      end if;
   end Get_Index;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Index : Source_File_Index) return String is
   begin
      return Filenames.Element (Index).all;
   end Get_Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File    : in out File_Type;
      Index   : Source_File_Index;
      Success : out Boolean)
   is

      procedure Try_Open
        (File    : in out File_Type;
         Name    : String;
         Success : out Boolean);
      --  Try to open Name, with no rebase/search information. In case of
      --  a success,

      --------------
      -- Try_Open --
      --------------

      procedure Try_Open
        (File    : in out File_Type;
         Name    : String;
         Success : out Boolean) is
      begin
         Open (File, In_File, Name);
         Success := True;
      exception
         when Name_Error =>
            Success := False;
      end Try_Open;

      Name : constant String := Get_Name (Index);
   begin
      --  Try original path

      Try_Open (File, Name, Success);

      --  Try to rebase

      if not Success then
         declare
            E : Source_Rebase_Entry_Acc := First_Source_Rebase_Entry;
            First : constant Positive := Name'First;
         begin
            while E /= null loop
               if Name'Length > E.Old_Prefix'Length
                 and then (Name (First .. First + E.Old_Prefix'Length - 1)
                           = E.Old_Prefix.all)
               then
                  Try_Open (File,
                            E.New_Prefix.all
                            & Name (First + E.Old_Prefix'Length
                                    .. Name'Last),
                            Success);
                  exit when Success;
               end if;
               E := E.Next;
            end loop;
         end;
      end if;

      --  Try source path

      if not Success then
         declare
            E : Source_Search_Entry_Acc := First_Source_Search_Entry;
         begin
            while E /= null loop
               Try_Open (File, E.Prefix.all & '/' & Name, Success);
               exit when Success;
               E := E.Next;
            end loop;
         end;
      end if;
   end Open;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (File  : Source_File_Index;
      State : Line_State;
      Line  : Natural;
      Info  : Addresses_Info_Acc;
      Base  : Traces_Base_Acc;
      Exec  : Exe_File_Acc)
   is
      Element : File_Info_Access renames File_Table.Table (File);
   begin
      Expand_Line_Table (File, Line);
      Append (Element.Lines.Table (Line), State, Info, Base, Exec);
   end Add_Line;

   ------------
   -- Append --
   ------------

   procedure Append
     (Info            : Line_Info_Access;
      State           : Line_State;
      Instruction_Set : Addresses_Info_Acc;
      Base            : Traces_Base_Acc;
      Exec            : Exe_File_Acc)
   is
      El : constant Line_Chain_Acc :=
        new Line_Chain'(OCI => (State => State,
                                Instruction_Set => Instruction_Set,
                                Base => Base,
                                Exec => Exec),
                        Next => null);
   begin
      if Info.First_Line = null then
         Info.First_Line := El;
      else
         Info.Last_Line.Next := El;
      end if;
      Info.Last_Line := El;
   end Append;

   -------------
   -- Element --
   -------------

   function Element
     (Lines : Source_Lines;
      Index : Natural)
     return Line_Info_Access is
   begin
      return Lines.Table (Index);
   end Element;

   -----------------------
   -- Expand_Line_Table --
   -----------------------

   procedure Expand_Line_Table (File : Source_File_Index; Line : Natural) is
      use Source_Line_Tables;
      FI           : File_Info_Access renames File_Table.Table (File);
      Current_Last : constant Natural := Last (FI.Lines);
   begin
      if Current_Last < Line then
         Set_Last (FI.Lines, Line);

         for J in Current_Last + 1 .. Line loop
            --  ??? This can certainly be improved; Traces_Sources.New_Line
            --  should be able to create new line infos.
            FI.Lines.Table (J) :=
              new Line_Info'(State => No_Code, others => <>);
            Traces_Sources.New_Line (File, J);
         end loop;
      end if;
   end Expand_Line_Table;

   -------------------------
   --  File_Table_Iterate --
   -------------------------

   procedure File_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index)) is
   begin
      for Index in File_Dynamic_Tables.First
        .. File_Dynamic_Tables.Last (File_Table)
      loop
         Process (Index);
      end loop;
   end File_Table_Iterate;

   ------------------------
   -- File_Table_Element --
   ------------------------

   function File_Table_Element
     (Index : Source_File_Index)
     return File_Info_Access is
   begin
      return File_Table.Table (Index);
   end File_Table_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Lines   : Source_Lines;
      Process : not null access procedure (Index : Natural)) is
   begin
      for Index in Source_Line_Tables.First
        .. Source_Line_Tables.Last (Source_Line_Tables.Instance (Lines))
      loop
         Process (Index);
      end loop;
   end Iterate;

   ---------------------
   -- New_Source_File --
   ---------------------

   procedure New_Source_File (File : Source_File_Index) is
      Last : Source_File_Index;
   begin
      Last := File_Dynamic_Tables.Last (File_Table);
      if File > Last then
         File_Dynamic_Tables.Set_Last (File_Table, File);
         for Index in Last + 1 .. File loop
            declare
               Line_Table : Source_Lines;
            begin
               Source_Line_Tables.Init
                 (Source_Line_Tables.Instance (Line_Table));
               File_Table.Table (Index) := new File_Info;
               File_Table.Table (Index).Lines := Line_Table;
               File_Table.Table (Index).Stats := (others => 0);
               File_Table.Table (Index).To_Display := False;
            end;
         end loop;
      end if;
      File_Table.Table (File).To_Display := True;
   end New_Source_File;

begin
   File_Dynamic_Tables.Init (File_Table);
end Files_Table;
