------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Doc_Generator.Requirements is

   --  Parse a source file containing the description of requirements
   --  and test cases to verify them
   procedure Parse_File (Path : String);

   --  Print the data structure containing all the parsed data
   procedure Print;

private

   --  constant TAGs --
   Requirement_Start_Tag : constant String := "--@req{";
   Requirement_End_Tag : constant String := "}";

   Driver_Tag : constant String := "--@driver";
   Driver_Id_Tag : constant String := "--@id{";
   Driver_Name_Tag : constant String := "--@test ";
   Driver_Expected_Tag : constant String := "--@expected";
   End_Tag : constant String := "--@end";

   --  a Target is a subprogram invoked by a test case
   type Target is tagged
      record
         ID : Ada.Strings.Unbounded.Unbounded_String;
         Subprogram : Ada.Strings.Unbounded.Unbounded_String;
         Description : Ada.Strings.Unbounded.Unbounded_String;
         Expected_Coverage : Function_Coverage;
      end record;

   type Target_Ref is access all Target'Class;

   package Target_List is new Ada.Containers.Doubly_Linked_Lists
     (Target_Ref);

   --  a Driver is a test case veryfing a requirement
   --  a Driver contains the list of targets it invokes
   type Driver is new Target with
      record
         Targets : Target_List.List := Target_List.Empty_List;
      end record;

   type Driver_Ref is access Driver;

   package Driver_List is new Ada.Containers.Doubly_Linked_Lists
     (Driver_Ref);

   --  a Requirement contains the det of drivers necessary for its
   --  verification
   type Requirement is
      record
         ID : Ada.Strings.Unbounded.Unbounded_String;
         Description : Ada.Strings.Unbounded.Unbounded_String;
         Drivers : Driver_List.List := Driver_List.Empty_List;
         In_File : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Requirement_Ref is access Requirement;

   package Requirement_List is new Ada.Containers.Doubly_Linked_Lists
     (Requirement_Ref);

   --  the list of requirements
   Req_List : Requirement_List.List := Requirement_List.Empty_List;

   File_Parsed : Boolean := False;

   Test_Cases_N : Natural := 0;

end Doc_Generator.Requirements;

