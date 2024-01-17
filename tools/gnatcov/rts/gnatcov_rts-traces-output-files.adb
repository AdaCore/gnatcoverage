------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with GNATcov_RTS.Strings; use GNATcov_RTS.Strings;

package body GNATcov_RTS.Traces.Output.Files is

   function Write_Trace_File_C
     (Buffers_Groups : GNATcov_RTS_Coverage_Buffers_Group_Array;
      Filename       : chars_ptr;
      Program_Name   : GNATcov_RTS_String;
      Exec_Date      : Unsigned_64;
      User_Data      : GNATcov_RTS_String) return int;
   pragma Import
     (C, Write_Trace_File_C, External_Name => "gnatcov_rts_write_trace_file");

   function Default_Trace_Filename_C
     (Env_Var : chars_ptr;
      Prefix  : chars_ptr;
      Tag     : chars_ptr;
      Simple  : unsigned) return chars_ptr;
   pragma Import
     (C, Default_Trace_Filename_C,
      External_Name => "gnatcov_rts_default_trace_filename");

   ----------------------------
   -- Default_Trace_Filename --
   ----------------------------

   function Default_Trace_Filename
     (Env_Var : String  := Default_Trace_Filename_Env_Var;
      Prefix  : String  := "gnatcov";
      Tag     : String  := "";
      Simple  : Boolean := False) return chars_ptr
   is
      Env_Var_C : chars_ptr := New_String (Env_Var);
      Prefix_C  : chars_ptr := New_String (Prefix);
      Tag_C     : chars_ptr := New_String (Tag);
      Simple_C  : constant unsigned  := Boolean'Pos (Simple);

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
      Exec_Date      : Time   := Clock;
      User_Data      : String := "") is
   begin
      if Write_Trace_File_C
           ((Buffers_Groups'Length, Buffers_Groups'Address),
            Filename,
            (Program_Name'Address, Program_Name'Length),
            Interfaces.Unsigned_64 (Exec_Date),
            (User_Data'Address, User_Data'Length)) = 1
      then
         raise IO_Error;
      end if;
   end Write_Trace_File;

   ------------------------------
   -- Write_Trace_File_Wrapper --
   ------------------------------

   procedure Write_Trace_File_Wrapper
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Filename       : chars_ptr := Default_Trace_Filename;
      Program_Name   : String := "unknown";
      Exec_Date      : Time   := Clock;
      User_Data      : String := "")
   is

      function C_Strerror (Errnum : C.int) return C.Strings.chars_ptr;
      pragma Import (C, C_Strerror, "strerror");
      --  strerror is already a function, we can bind to its symbol directly

      function C_Errno return C.Int;
      pragma Import (C, C_Errno, "gnatcov_rts_get_errno");
      --  Get the errno value through the wrapper declared in
      --  gnatcov_rts_c-os_interface.h.

   begin
      Write_Trace_File
        (Buffers_Groups, Filename, Program_Name, Exec_Date, User_Data);
   exception
      when IO_Error =>
         declare
            C_Error_Msg : constant chars_ptr := C_Strerror (C_Errno);
            Error_Msg   : constant String := Value (C_Error_Msg);
         begin
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "Error occurred while creating the trace file "
               & Value (Filename) & ": " & Error_Msg);
         end;
   end Write_Trace_File_Wrapper;

end GNATcov_RTS.Traces.Output.Files;
