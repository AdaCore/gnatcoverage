------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;

with GNATcov_RTS.Traces.Output.Bytes_IO;

package body GNATcov_RTS.Traces.Output.Files is

   package BIO renames Output.Bytes_IO;  --  Just a shorthand

   type Process_Id is new Interfaces.Unsigned_64;
   function Current_Process_Id return Process_Id;
   pragma Import (C, Current_Process_Id, "gnatcov_rts_getpid");

   procedure Write_Bytes
     (File  : in out BIO.File_Type;
      Bytes : System.Address;
      Count : Natural);
   --  Callback for GNATcov_RTS.Traces.Output.Generic_Write_Trace_File

   function Basename (Name : String) return String;
   --  Return the base name of the Name file.
   --
   --  Note that this unit must be compilable with an Ada 95 compiler, so this
   --  is a good enough replacement of Ada.Directories.Simple_Name (introduced
   --  in Ada 2005).

   function Environment_Variable (Name : String) return String;
   --  Return the value for the Name environment variable. Return an empty
   --  string if there is no matching environment variable.
   --
   --  Note that this unit must be compilable with an Ada 95 compiler, so this
   --  is a good enough replacement of Ada.Environment_Variables (introduced in
   --  Ada 2005).

   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";
   function Hex_Image (Value : Unsigned_64) return String;
   --  Return Timestamp as an hexadecimal string

   function Default_Trace_Basename
     (Prefix : String;
      Tag    : String := "";
      Simple : Boolean := False) return String;
   --  Helper for Default_Trace_Filename, to be called when the environment
   --  variable does not provide the source trace filename. Return the basename
   --  for the source trace file.

   --------------
   -- Basename --
   --------------

   function Basename (Name : String) return String is
      First : Natural := Name'Last + 1;
   begin
      for J in reverse Name'Range loop
         exit when Name (J) = '/' or Name (J) = '\';
         First := J;
      end loop;
      return Name (First .. Name'Last);
   end Basename;

   --------------------------
   -- Environment_Variable --
   --------------------------

   function Environment_Variable (Name : String) return String is
      use Interfaces.C.Strings;

      function getenv (Name : chars_ptr) return chars_ptr;
      pragma Import (C, getenv);

      C_Name : chars_ptr          := New_String (Name);
      Result : constant chars_ptr := getenv (C_Name);
   begin
      Free (C_Name);
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Environment_Variable;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Value : Unsigned_64) return String is
      Remaining  : Unsigned_64 := Value;
      Result     : String (1 .. 16);
      Next_Digit : Positive := Result'Last;
   begin
      --  Store the actual result in Result (Next_Digit - 1 .. Result'Last),
      --  updating Next_Digit as we progress (Remaining is updated each time
      --  we add a digit).

      if Value = 0 then
         return "0";
      end if;

      while Remaining /= 0 loop
         Result (Next_Digit) :=
            Hex_Digits (Natural (Remaining mod 16));
         Remaining := Remaining / 16;
         Next_Digit := Next_Digit - 1;
      end loop;
      return Result (Next_Digit + 1 .. Result'Last);
   end Hex_Image;

   ----------------------------
   -- Default_Trace_Basename --
   ----------------------------

   function Default_Trace_Basename
     (Prefix : String;
      Tag    : String := "";
      Simple : Boolean := False) return String
   is
      Extension : constant String := ".srctrace";
   begin
      if Simple then
         return Prefix & Extension;
      end if;

      declare
         Suffix : constant String :=
           "-" & Hex_Image (Unsigned_64 (Current_Process_Id))
           & "-" & Hex_Image (Unsigned_64 (Clock))
           & Extension;
      begin
         if Tag = "" then
            return Prefix & Suffix;
         else
            return Prefix & "-" & Tag & Suffix;
         end if;
      end;
   end Default_Trace_Basename;

   ----------------------------
   -- Default_Trace_Filename --
   ----------------------------

   function Default_Trace_Filename
     (Env_Var : String := Default_Trace_Filename_Env_Var;
      Prefix  : String := "gnatcov";
      Tag     : String := "";
      Simple  : Boolean := False) return String
   is
      Env_Trace_Filename : constant String := Environment_Variable (Env_Var);
   begin
      if Env_Trace_Filename = "" then
         return Default_Trace_Basename (Prefix, Tag, Simple);

      elsif Env_Trace_Filename (Env_Trace_Filename'Last) = '/'
        or else Env_Trace_Filename (Env_Trace_Filename'Last) = '\'
      then
         return Env_Trace_Filename
                & Default_Trace_Basename (Prefix, Tag, Simple);

      else
         return Env_Trace_Filename;
      end if;
   end Default_Trace_Filename;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      function time return Time;
      pragma Import (C, time, "gnatcov_rts_time_to_uint64");
   begin
      return time;
   end Clock;

   -----------------
   -- Format_Date --
   -----------------

   function Format_Date (Timestamp : Time) return Serialized_Timestamp is
      Result : Serialized_Timestamp;
      TS     : Time := Timestamp;
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (TS mod 2**8);
         TS         := TS / 2**8;
      end loop;
      return Result;
   end Format_Date;

   -----------------
   -- Write_Bytes --
   -----------------

   procedure Write_Bytes
     (File  : in out BIO.File_Type;
      Bytes : System.Address;
      Count : Natural)
   is
      type Uint8_Array is array (Positive range <>) of Interfaces.Unsigned_8;
      Content : Uint8_Array (1 .. Count);
      for Content'Address use Bytes;
      pragma Import (Ada, Content);
   begin
      for I in Content'Range loop
         BIO.Write (File, Content (I));
      end loop;
      pragma Unreferenced (File);
   end Write_Bytes;

   procedure Write_Trace_File is new
     GNATcov_RTS.Traces.Output.Generic_Write_Trace_File (BIO.File_Type);

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Filename     : String := Default_Trace_Filename;
      Program_Name : String := Ada.Command_Line.Command_Name;
      Exec_Date    : Time := Clock;
      User_Data    : String := "")
   is
      File : BIO.File_Type;
   begin
      if Filename = "" then
         BIO.Create (File, Name => Default_Trace_Filename (Prefix => ""));
      else
         BIO.Create (File, Name => Filename);
      end if;
      Write_Trace_File
        (File, Buffers, Program_Name, Format_Date (Exec_Date), User_Data);
      BIO.Close (File);
   end Write_Trace_File;

end GNATcov_RTS.Traces.Output.Files;
