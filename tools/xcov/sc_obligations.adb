------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

--  Source Coverage Obligations

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with SCOs;
with Get_SCOs;

package body SC_Obligations is

   procedure Load_SCOs_From_ALI (ALI_Filename : String);
   --  Load SCOs from the named ALI file, populating a map of slocs to SCOs

   -------------------------------
   -- Main SCO descriptor table --
   -------------------------------

   type SCO_Descriptor is record
      Kind       : SCO_Kind;
      First_Sloc : Source_Location;
      Last_Sloc  : Source_Location;
   end record;
   subtype Valid_SCO_Id is SCO_Id range No_SCO_Id + 1 .. SCO_Id'Last;

   package SCO_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Valid_SCO_Id,
        Element_Type => SCO_Descriptor);
   SCO_Vector : SCO_Vectors.Vector;

   --------------------------
   -- Sloc -> SCO_Id index --
   --------------------------

   package Sloc_To_SCO_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_Location,
      Element_Type => SCO_Id);
   Sloc_To_SCO_Map : Sloc_To_SCO_Maps.Map;

   ----------------
   -- First_Sloc --
   ----------------

   function First_Sloc (SCO : SCO_Id) return Source_Location is
      pragma Unreferenced (SCO);
   begin
      --  Not implemented yet???

      return (First_Source_File, 0, 0);
   end First_Sloc;

   -----------
   -- Image --
   -----------

   function Image (SCO : SCO_Id) return String is
   begin
      return Trim (SCO'Img, Side => Ada.Strings.Both);
   end Image;

   ----------
   -- Kind --
   ----------

   function Kind (SCO : SCO_Id) return SCO_Kind is
      pragma Unreferenced (SCO);
   begin
      --  Not implemented yet???

      return SCO_Kind'First;
   end Kind;

   ---------------
   -- Last_Sloc --
   ---------------

   function Last_Sloc (SCO : SCO_Id) return Source_Location is
      pragma Unreferenced (SCO);
   begin
      --  Not implemented yet???

      return (First_Source_File, 0, 0);
   end Last_Sloc;

   ---------------
   -- Load_SCOs --
   ---------------

   procedure Load_SCOs (ALI_List_Filename : String_Acc) is
      ALI_List : File_Type;
   begin
      if ALI_List_Filename = null then
         return;
      end if;
      Open (ALI_List, In_File, ALI_List_Filename.all);
      while not End_Of_File (ALI_List) loop
         declare
            Line : String (1 .. 1024);
            Last : Natural;
         begin
            Get_Line (ALI_List, Line, Last);
            Load_SCOs_From_ALI (Line (1 .. Last));
         end;
      end loop;
   end Load_SCOs;

   ------------------------
   -- Load_SCOs_From_ALI --
   ------------------------

   procedure Load_SCOs_From_ALI (ALI_Filename : String) is
      Cur_SCO_Unit  : SCOs.Unit_Index;

      Cur_Source_File : Source_File_Index;

      ALI_File : File_Type;
      Line : String (1 .. 1024);
      Last : Natural;
      Index : Natural;

      function Getc return Character;
      --  Consume and return one character from Line.
      --  Load next line if at end of line. Return ^Z if at end of file.

      function Nextc return Character;
      --  Peek at current character in Line

      ----------
      -- Getc --
      ----------

      function Getc return Character is
         Next_Char : constant Character := Nextc;
      begin
         Index := Index + 1;
         if Index > Last then
            Get_Line (ALI_File, Line, Last);
            Index := 1;
         end if;
         return Next_Char;
      end Getc;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         if End_Of_File (ALI_File) then
            return Character'Val (16#1a#);
         end if;
         return Line (Index);
      end Nextc;

      function Get_SCOs_From_ALI is new Get_SCOs;

   begin
      Open (ALI_File, In_File, ALI_Filename);
      Scan_ALI : loop
         Get_Line (ALI_File, Line, Last);
         case Line (1) is
            when 'U' =>
               --  Scan past unit name and whitespace

               Index := 3;
               while Line (Index) /= ' ' loop
                  Index := Index + 1;
               end loop;

               while Line (Index) = ' ' loop
                  Index := Index + 1;
               end loop;

               Last := Index;
               while Line (Last) /= ' ' loop
                  Last := Last + 1;
               end loop;

               Cur_Source_File := Get_Index (Line (Index .. Last - 1));

            when 'C' =>
               exit Scan_ALI;

            when others =>
               null;
         end case;
      end loop Scan_ALI;

      Index := 1;

      Cur_SCO_Unit := Get_SCOs_From_ALI;

      --  Walk low-level SCO table for this unit and populate high-level tables

      for Cur_SCO_Entry in SCOs.SCO_Unit_Table.Table (Cur_SCO_Unit).From
                        .. SCOs.SCO_Unit_Table.Table (Cur_SCO_Unit).To
      loop
         Process_Entry : declare
            SCOE : SCOs.SCO_Table_Entry renames
                                     SCOs.SCO_Table.Table (Cur_SCO_Entry);

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location) return Source_Location;
            --  Build a Sources.Source_Location record from the low-level
            --  SCO Sloc info.

            ---------------
            -- Make_Sloc --
            ---------------

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location) return Source_Location is
            begin
               return Source_Location'
                 (Source_File => Cur_Source_File,
                  Line        => Natural (SCO_Source_Loc.Line),
                  Column      => Natural (SCO_Source_Loc.Col));
            end Make_Sloc;

            Kind : SCO_Kind;

         begin
            case SCOE.C1 is
               when 'S' =>
                  Kind := Statement;
               when 'I' | 'E' | 'W' | 'X' =>
                  Kind := Decision;
               when others =>
                  goto End_Process_Entry;
            end case;

            SCO_Vector.Append
              (SCO_Descriptor'(Kind       => Kind,
                               First_Sloc => Make_Sloc (SCOE.From),
                               Last_Sloc  => Make_Sloc (SCOE.To)));

            Sloc_To_SCO_Map.Insert
              (Make_Sloc (SCOE.From), SCO_Vector.Last_Index);
         <<End_Process_Entry>>
            null;
         end Process_Entry;
      end loop;
   end Load_SCOs_From_ALI;

   -----------------
   -- Sloc_To_SCO --
   -----------------

   function Sloc_To_SCO (Sloc : Source_Location) return SCO_Id is
      use Sloc_To_SCO_Maps;
      Cur : constant Cursor := Sloc_To_SCO_Map.Floor (Sloc);
   begin
      if Cur /= No_Element then
         declare
            SCO : constant SCO_Id := Element (Cur);
         begin
            if Sloc <= Last_Sloc (SCO) then
               return SCO;
            end if;
         end;
      end if;
      return No_SCO_Id;
   end Sloc_To_SCO;

end SC_Obligations;
