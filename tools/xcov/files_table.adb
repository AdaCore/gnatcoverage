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

package body Files_Table is

   subtype Valid_Source_File_Index is
     Source_File_Index range First_Source_File .. Source_File_Index'Last;
   package File_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_Source_File_Index,
      Element_Type => File_Info_Access);

   Files_Table : File_Vectors.Vector;

   procedure Expand_Line_Table (File : Source_File_Index; Line : Positive);
   --  If Line is not in File's line table, expand this table and mark the new
   --  line as No_Code.

   procedure Append_Object_Coverage
     (Info            : Line_Info_Access;
      State           : Line_State;
      Instruction_Set : Addresses_Info_Acc;
      Base            : Traces_Base_Acc;
      Exec            : Exe_File_Acc);
   --  Append object coverage information to a source line.

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
            Info : constant File_Info_Access :=
              new File_Info'(File_Name => New_Name,
                             Lines => (Source_Line_Vectors.Empty_Vector
                                         with null record),
                             Stats => (others => 0),
                             To_Display => False);
         begin
            Files_Table.Append (Info);
            Filename_Map.Insert (New_Name, Files_Table.Last_Index);
            return Files_Table.Last_Index;
         end;
      end if;
   end Get_Index;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Index : Source_File_Index) return String is
   begin
      return Files_Table.Element (Index).File_Name.all;
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

   procedure Add_Line_For_Object_Coverage
     (File  : Source_File_Index;
      State : Line_State;
      Line  : Positive;
      Info  : Addresses_Info_Acc;
      Base  : Traces_Base_Acc;
      Exec  : Exe_File_Acc)
   is
      Element : File_Info_Access renames Files_Table.Element (File);
   begin
      Expand_Line_Table (File, Line);
      Append_Object_Coverage (Element.Lines.Element (Line),
                              State, Info, Base, Exec);
   end Add_Line_For_Object_Coverage;

   ----------------------------
   -- Append_Object_Coverage --
   ----------------------------

   procedure Append_Object_Coverage
     (Info            : Line_Info_Access;
      State           : Line_State;
      Instruction_Set : Addresses_Info_Acc;
      Base            : Traces_Base_Acc;
      Exec            : Exe_File_Acc)
   is
      El : constant Object_Coverage_Info_Acc :=
        new Object_Coverage_Info'(State => State,
                                  Instruction_Set => Instruction_Set,
                                  Base => Base,
                                  Exec => Exec,
                                  Next => null);
   begin
      if Info.Obj_First = null then
         Info.Obj_First := El;
      else
         Info.Obj_Last.Next := El;
      end if;
      Info.Obj_Last := El;
   end Append_Object_Coverage;

   -------------------
   -- Get_Line_Info --
   -------------------

   function Get_Line_Info
     (File  : File_Info_Access;
      Index : Positive)
     return Line_Info_Access is
   begin
      return File.Lines.Element (Index);
   end Get_Line_Info;

   -----------------------
   -- Expand_Line_Table --
   -----------------------

   procedure Expand_Line_Table (File : Source_File_Index; Line : Positive) is
      FI           : File_Info_Access renames Files_Table.Element (File);
   begin
      while FI.Lines.Last_Index < Line loop
         FI.Lines.Append (new Line_Info'(State => No_Code, others => <>));
      end loop;
   end Expand_Line_Table;

   -------------------------
   --  Files_Table_Iterate --
   -------------------------

   procedure Files_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index)) is
   begin
      for Index in Files_Table.First_Index .. Files_Table.Last_Index loop
         Process (Index);
      end loop;
   end Files_Table_Iterate;

   ------------------------
   -- Files_Table_Element --
   ------------------------

   function Files_Table_Element
     (Index : Source_File_Index)
     return File_Info_Access is
   begin
      return Files_Table.Element (Index);
   end Files_Table_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate_On_Lines
     (File   : File_Info_Access;
      Process : not null access procedure (Index : Positive)) is
   begin
      for Index in File.Lines.First_Index .. File.Lines.Last_Index loop
         Process (Index);
      end loop;
   end Iterate_On_Lines;

   ---------------------
   -- New_Source_File --
   ---------------------

   procedure New_Source_File (File : Source_File_Index) is
   begin
      Files_Table.Element (File).all.To_Display := True;
   end New_Source_File;
end Files_Table;
