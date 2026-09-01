------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit needs to be compilable with Ada 95 compilers

with Interfaces.C; use Interfaces.C;

with GNATcov_RTS.Strings; use GNATcov_RTS.Strings;

package body GNATcov_RTS.Traces.Output.Files is

   procedure Write_Trace_File_C
     (Buffers_Groups : GNATcov_RTS_Coverage_Buffers_Group_Array;
      Filename       : chars_ptr;
      Program_Name   : GNATcov_RTS_String;
      Exec_Date      : Unsigned_64;
      User_Data      : GNATcov_RTS_String);
   pragma
     Import
       (C,
        Write_Trace_File_C,
        External_Name => "gnatcov_rts_write_trace_file");

   function Default_Trace_Filename_C
     (Env_Var : chars_ptr;
      Prefix  : chars_ptr;
      Tag     : chars_ptr;
      Simple  : unsigned) return chars_ptr;
   pragma
     Import
       (C,
        Default_Trace_Filename_C,
        External_Name => "gnatcov_rts_default_trace_filename");

   ----------------------------
   -- Default_Trace_Filename --
   ----------------------------

   function Default_Trace_Filename
     (Env_Var : String := Default_Trace_Filename_Env_Var;
      Prefix  : String := "gnatcov";
      Tag     : String := "";
      Simple  : Boolean := False) return chars_ptr
   is
      Env_Var_C : chars_ptr := New_String (Env_Var);
      Prefix_C  : chars_ptr := New_String (Prefix);
      Tag_C     : chars_ptr := New_String (Tag);
      Simple_C  : constant unsigned := Boolean'Pos (Simple);

      Result : constant chars_ptr :=
        Default_Trace_Filename_C (Env_Var_C, Prefix_C, Tag_C, Simple_C);
   begin
      Free (Env_Var_C);
      Free (Prefix_C);
      Free (Tag_C);
      return Result;
   end Default_Trace_Filename;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      function Time return Time;
      pragma Import (C, Time, "gnatcov_rts_time_to_uint64");
   begin
      return Time;
   end Clock;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Filename       : chars_ptr := Default_Trace_Filename;
      Program_Name   : String := "unknown";
      Exec_Date      : Time := Clock;
      User_Data      : String := "")
   is
      --  See the note about -gnatw.X in gnatcov_rts.gpr

      pragma Warnings (Off);
      Buffers_Groups_C : constant GNATcov_RTS_Coverage_Buffers_Group_Array :=
        (Buffers_Groups'Length, Buffers_Groups'Address);
      Program_Name_C   : constant GNATcov_RTS_String :=
        (Program_Name'Address, Program_Name'Length);
      Exec_Date_C      : constant Unsigned_64 :=
        Interfaces.Unsigned_64 (Exec_Date);
      User_Data_C      : constant GNATcov_RTS_String :=
        (User_Data'Address, User_Data'Length);
      pragma Warnings (On);
   begin
      Write_Trace_File_C
        (Buffers_Groups_C, Filename, Program_Name_C, Exec_Date_C, User_Data_C);
   end Write_Trace_File;

end GNATcov_RTS.Traces.Output.Files;
