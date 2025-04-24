------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Ada.Containers.Vectors;

with Coverage.Source; use Coverage.Source;
with Logging;
with SC_Obligations;  use SC_Obligations;
with Slocs;           use Slocs;
with Strings;         use Strings;

package LLVM_JSON_Checkpoints is

   LLVM_Trace : constant Logging.GNATCOLL_Trace :=
      Logging.Create_Trace ("LLVM-JSON");
   --  Trace to log LLVM JSON Trace Adapter messages.

   type LLVM_Region_Kind is (Statement, Decision, Condition);

   type LLVM_Region_Id is new Natural;
   No_LLVM_Region_Id : constant LLVM_Region_Id := 0;
   subtype Valid_LLVM_Region_Id is LLVM_Region_Id
      range No_LLVM_Region_Id + 1 .. LLVM_Region_Id'Last;

   type LLVM_Region (Kind : LLVM_Region_Kind := Statement) is record
      Span : Local_Source_Location_Range;
      Id   : Valid_LLVM_Region_Id;
      SCO  : SCO_Id := No_SCO_Id;
      --  SCO_Id is meant to be set by the SC_obligations pass when
      --  registering SCOs.

      case Kind is
         when Statement =>
            Execution_Count : Natural;

         when Decision =>
            Num_Conditions : Natural;
            Test_Vectors   : Evaluation_Vectors.Vector;
            Next_Condition : Condition_Index := Condition_Index'First;
            --  Used for assigning a condition index to its conditions

         when Condition =>
            Parent_Id : Valid_LLVM_Region_Id;
            Index     : Condition_Index;
      end case;
   end record;

   package LLVM_Region_Vector is new Ada.Containers.Vectors
     (Index_Type   => Valid_LLVM_Region_Id,
      Element_Type => LLVM_Region);

   type LLVM_Coverage_Function_Ckpt is record
      Name         : Unbounded_String;
      Mangled_Name : Unbounded_String;
      Regions      : LLVM_Region_Vector.Vector;
   end record;

   package LLVM_Coverage_Function_Ckpt_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => LLVM_Coverage_Function_Ckpt);

   type LLVM_Coverage_File_Ckpt is record
      Filename  : Unbounded_String;
      Functions : LLVM_Coverage_Function_Ckpt_Vector.Vector;
   end record;

   package LLVM_Coverage_File_Ckpt_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => LLVM_Coverage_File_Ckpt);

   type LLVM_Coverage_Ckpt is record
      JSON_Filename : Unbounded_String;
      File_Reports  : LLVM_Coverage_File_Ckpt_Vector.Vector;
   end record;

   procedure JSON_Load (JSON_Filename : String);
   --  Read a JSON file containing LLVM coverage data
   --  and populate internal structures with it.

end LLVM_JSON_Checkpoints;
