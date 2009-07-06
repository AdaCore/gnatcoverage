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

with SCOs;  use SCOs;
with Types; use Types;
with Get_SCOs;

package body SC_Obligations is

   subtype Source_Location is Sources.Source_Location;
   No_Location : Source_Location renames Sources.No_Location;
   --  (not SCOs.Source_Location)

   procedure Load_SCOs_From_ALI (ALI_Filename : String);
   --  Load SCOs from the named ALI file, populating a map of slocs to SCOs

   -------------------------------
   -- Main SCO descriptor table --
   -------------------------------

   type SCO_Descriptor (Kind : SCO_Kind := SCO_Kind'First) is record
      First_Sloc : Source_Location;
      --  First sloc (for a complex decision, taken from first condition)

      Last_Sloc  : Source_Location;
      --  Last sloc (unset for complex decisions)

      Parent : SCO_Id := No_SCO_Id;
      --  For a decision, pointer to the enclosing statement (or condition in
      --  the case of a nested decision), unset if decision is part of a
      --  flow control structure.
      --  For a condition, pointer to the enclosing decision.

      case Kind is
         when Decision =>
            Is_Complex_Decision : Boolean;
            --  True for complex decisions.
            --  Note that there is always a distinct Condition SCO descriptor,
            --  even for simple decisions.
         when others =>
            null;
      end case;
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
   begin
      return SCO_Vector.Element (SCO).First_Sloc;
   end First_Sloc;

   -----------
   -- Image --
   -----------

   function Image (SCO : SCO_Id) return String is
   begin
      if SCO = No_SCO_Id then
         return "<no SCO>";
      else
         declare
            SCOD : constant SCO_Descriptor := SCO_Vector.Element (SCO);
         begin
            return "SCO #" & Trim (SCO'Img, Side => Ada.Strings.Both) & ": "
              & SCO_Kind'Image (SCOD.Kind) & " at "
              & Image (SCOD.First_Sloc) & "-" & Image (SCOD.Last_Sloc);
         end;
      end if;
   end Image;

   ----------
   -- Kind --
   ----------

   function Kind (SCO : SCO_Id) return SCO_Kind is
   begin
      return SCO_Vector.Element (SCO).Kind;
   end Kind;

   ---------------
   -- Last_Sloc --
   ---------------

   function Last_Sloc (SCO : SCO_Id) return Source_Location is
   begin
      return SCO_Vector.Element (SCO).Last_Sloc;
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
      Cur_Source_File : Source_File_Index := No_Source_File;
      Cur_SCO_Unit : SCO_Unit_Index;
      Last_Entry_In_Cur_Unit : Int;

      ALI_File : File_Type;
      Line : String (1 .. 1024);
      Last : Natural;
      Index : Natural;

      Current_Complex_Decision : SCO_Id := No_SCO_Id;

      Last_SCO_Upon_Entry : constant SCO_Id := SCO_Vector.Last_Index;

      function Getc return Character;
      --  Consume and return one character from Line.
      --  Load next line if at end of line. Return ^Z if at end of file.

      function Nextc return Character;
      --  Peek at current character in Line

      procedure Skipc;
      --  Skip one character in Line

      ----------
      -- Getc --
      ----------

      function Getc return Character is
         Next_Char : constant Character := Nextc;
      begin
         Index := Index + 1;
         if Index > Last + 1 then
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
         if Index = Last + 1 then
            return ASCII.LF;
         end if;
         return Line (Index);
      end Nextc;

      -----------
      -- Skipc --
      -----------

      procedure Skipc is
         C : Character;
         pragma Unreferenced (C);
      begin
         C := Getc;
      end Skipc;

      procedure Get_SCOs_From_ALI is new Get_SCOs;

   begin
      Open (ALI_File, In_File, ALI_Filename);
      Scan_ALI : loop
         if End_Of_File (ALI_File) then
            --  No SCOs in this ALI

            Close (ALI_File);
            return;
         end if;

         Get_Line (ALI_File, Line, Last);
         case Line (1) is
            when 'C' =>
               exit Scan_ALI;

            when others =>
               null;
         end case;
      end loop Scan_ALI;

      Index := 1;

      Get_SCOs_From_ALI;

      --  Walk low-level SCO table for this unit and populate high-level tables

      Cur_SCO_Unit := SCO_Unit_Table.First;
      Last_Entry_In_Cur_Unit := SCOs.SCO_Table.First - 1;
      --  Note, the first entry in the SCO_Unit_Table is unused

      for Cur_SCO_Entry in
        SCOs.SCO_Table.First .. SCOs.SCO_Table.Last
      loop
         if Cur_SCO_Entry > Last_Entry_In_Cur_Unit then
            Cur_SCO_Unit := Cur_SCO_Unit + 1;
            pragma Assert
              (Cur_SCO_Unit in SCOs.SCO_Unit_Table.First
                            .. SCOs.SCO_Unit_Table.Last);
            declare
               SCOUE : SCO_Unit_Table_Entry
                         renames SCOs.SCO_Unit_Table.Table (Cur_SCO_Unit);
            begin
               pragma Assert (Cur_SCO_Entry in SCOUE.From .. SCOUE.To);
               Last_Entry_In_Cur_Unit := SCOUE.To;
               Cur_Source_File := Get_Index (SCOUE.File_Name.all);
            end;
         end if;

         pragma Assert (Cur_Source_File /= No_Source_File);
         Process_Entry : declare
            SCOE : SCOs.SCO_Table_Entry renames
                                     SCOs.SCO_Table.Table (Cur_SCO_Entry);

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location) return Source_Location;
            --  Build a Sources.Source_Location record from the low-level
            --  SCO Sloc info.

            procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor);
            --  Update the first sloc of a complex decision SCOD from that
            --  of its first condition (which is the current SCOE).

            ---------------
            -- Make_Sloc --
            ---------------

            function Make_Sloc
              (SCO_Source_Loc : SCOs.Source_Location) return Source_Location
            is
            begin
               if SCO_Source_Loc = SCOs.No_Source_Location then
                  return Source_Location'
                    (Source_File => No_Source_File, others => <>);
               end if;

               return Source_Location'
                 (Source_File => Cur_Source_File,
                  Line        => Natural (SCO_Source_Loc.Line),
                  Column      => Natural (SCO_Source_Loc.Col));
            end Make_Sloc;

            --------------------------
            -- Update_Decision_Sloc --
            --------------------------

            procedure Update_Decision_Sloc (SCOD : in out SCO_Descriptor) is
            begin
               if SCOD.First_Sloc.Source_File = No_Source_File then
                  SCOD.First_Sloc := Make_Sloc (SCOE.From);
               end if;
            end Update_Decision_Sloc;

         begin
            case SCOE.C1 is
               when 'S' =>
                  --  Statement

                  pragma Assert (Current_Complex_Decision = No_SCO_Id);
                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Statement,
                                     First_Sloc => Make_Sloc (SCOE.From),
                                     Last_Sloc  => Make_Sloc (SCOE.To),
                                     others     => <>));

               when 'I' | 'E' | 'W' | 'X' =>
                  --  Decision

                  pragma Assert (Current_Complex_Decision = No_SCO_Id);
                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Decision,
                                     First_Sloc => Make_Sloc (SCOE.From),
                                     Last_Sloc  => Make_Sloc (SCOE.To),
                                     Is_Complex_Decision =>
                                                   not SCOE.Last,
                                     others     => <>));

                  if SCOE.Last then
                     --  Simple decision: no separate condition SCOE, create
                     --  condition immediately.

                     SCO_Vector.Append
                       (SCO_Descriptor'(Kind       => Condition,
                                        First_Sloc => Make_Sloc (SCOE.From),
                                        Last_Sloc  => Make_Sloc (SCOE.To),
                                        Parent     => SCO_Vector.Last_Index,
                                        others     => <>));

                  else
                     --  Complex decision: conditions appear as distinct SCOEs

                     Current_Complex_Decision := SCO_Vector.Last_Index;
                  end if;

               when ' ' =>
                  --  Condition

                  pragma Assert (Current_Complex_Decision /= No_SCO_Id);

                  SCO_Vector.Update_Element
                    (Index   => Current_Complex_Decision,
                     Process => Update_Decision_Sloc'Access);

                  SCO_Vector.Append
                    (SCO_Descriptor'(Kind       => Decision,
                                     First_Sloc => Make_Sloc (SCOE.From),
                                     Last_Sloc  => Make_Sloc (SCOE.To),
                                     Parent     => Current_Complex_Decision,
                                     others     => <>));

                  if SCOE.Last then
                     Current_Complex_Decision := No_SCO_Id;
                  end if;

               when '!' | '^' | '&' | '|' =>
                  --  Operator

                  null;

               when 'T' =>
                  --  Exit point

                  null;

               when others =>
                  raise Program_Error
                    with "unexpected SCO entry code: " & SCOE.C1;
            end case;

         end Process_Entry;
      end loop;

      --  Build Sloc -> SCO index and set up Parent links

      for J in Last_SCO_Upon_Entry + 1 .. SCO_Vector.Last_Index loop
         declare
            First : Source_Location := SCO_Vector.Element (J).First_Sloc;

            procedure Process_Descriptor (SCOD : in out SCO_Descriptor);
            --  Set up parent link for SCOD at index J, and insert Sloc -> SCO
            --  map entry.

            procedure Process_Descriptor (SCOD : in out SCO_Descriptor) is
               Enclosing_SCO : constant SCO_Id := Sloc_To_SCO (First);
            begin
               case SCOD.Kind is

                  when Decision =>
                     --  A Decision SCO must have a statement or (in the case
                     --  of a nested decision) a Condition SCO as its parent,
                     --  or no parent at all.

                     pragma Assert (Enclosing_SCO = No_SCO_Id
                                      or else
                                    Kind (Enclosing_SCO) /= Decision);
                     SCOD.Parent := Enclosing_SCO;

                     --  Decisions are not included in the sloc map, instead
                     --  their conditions are.

                     First := No_Location;
                  when Statement =>
                     --  A SCO for a (simple) statement is never nested

                     pragma Assert (Enclosing_SCO = No_SCO_Id);
                     null;

                  when Condition =>
                     --  Parent is already set to the enclosing decision
                     null;

               end case;

               if First /= No_Location then
                  Sloc_To_SCO_Map.Insert (First, J);
               end if;
            end Process_Descriptor;
         begin
            SCO_Vector.Update_Element (J, Process_Descriptor'Access);
         end;
      end loop;

      Close (ALI_File);
   end Load_SCOs_From_ALI;

   -----------------
   -- Sloc_To_SCO --
   -----------------

   function Sloc_To_SCO (Sloc : Source_Location) return SCO_Id is
      use Sloc_To_SCO_Maps;
      Cur : constant Cursor := Sloc_To_SCO_Map.Floor (Sloc);
      SCO : SCO_Id;
   begin
      if Cur /= No_Element then
         SCO := Element (Cur);
      else
         SCO := No_SCO_Id;
      end if;

      while SCO /= No_SCO_Id loop
         exit when Sloc <= Last_Sloc (SCO);
         SCO := SCO_Vector.Element (SCO).Parent;
      end loop;

      return SCO;
   end Sloc_To_SCO;

end SC_Obligations;
