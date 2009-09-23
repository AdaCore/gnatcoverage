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

with Traces_Sources;

package body File_Tables is

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
end File_Tables;
