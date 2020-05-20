--  This unit needs to be compilable with Ada 95 compilers

with Ada.Calendar.Conversions;
with Ada.Command_Line;
with Ada.Direct_IO;

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

with System;

with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;

package body GNATcov_RTS.Traces.Output.Files is

   package Bytes_IO is new Ada.Direct_IO (Interfaces.Unsigned_8);

   procedure Write_Bytes
     (File  : in out Bytes_IO.File_Type;
      Bytes : System.Address;
      Count : Natural);
   --  Callback for GNATcov_RTS.Traces.Output.Generic_Write_Trace_File

   ----------------------------
   -- Default_Trace_Filename --
   ----------------------------

   function Default_Trace_Filename return String is

      --  We need this unit to be compileable in Ada 95 mode, so we cannot
      --  use:
      --
      --  * Ada.Environment_Variables: directly use the libc's getenv function.
      --  * Ada.Directories.Simple_Name: do a good enough approximation
      --    instead.

      use Interfaces.C.Strings;

      function Environment_Variable (Name : String) return String;
      --  Return the value for the Name environment variable. Return an empty
      --  string if there is no matching environment variable.

      function Basename (Name : String) return String;
      --  Return the base name of the Name file

      --------------------------
      -- Environment_Variable --
      --------------------------

      function Environment_Variable (Name : String) return String is
         function getenv (Name : chars_ptr) return chars_ptr;
         pragma Import (C, getenv);

         C_Name : chars_ptr := New_String (Name);
         Result : constant chars_ptr := getenv (C_Name);
      begin
         Free (C_Name);
         if Result = Null_Ptr then
            return "";
         else
            return Value (Result);
         end if;
      end Environment_Variable;

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

      Env_Trace_Filename : constant String :=
         Environment_Variable (GNATCOV_TRACE_FILE);

   begin
      if Env_Trace_Filename /= "" then
         return Env_Trace_Filename;
      else
         return Basename (Ada.Command_Line.Command_Name) & ".srctrace";
      end if;
   end Default_Trace_Filename;

   -----------------
   -- Format_Date --
   -----------------

   function Format_Date (Date : Ada.Calendar.Time) return Serialized_Timestamp
   is
      use Ada.Calendar;
      use Interfaces.C;
      Timestamp : long := Ada.Calendar.Conversions.To_Unix_Time (Date);
      Result    : Serialized_Timestamp;
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Timestamp mod 2 ** 8);
         Timestamp := Timestamp / 2 ** 8;
      end loop;
      return Result;
   end Format_Date;

   -----------------
   -- Write_Bytes --
   -----------------

   procedure Write_Bytes
     (File  : in out Bytes_IO.File_Type;
      Bytes : System.Address;
      Count : Natural)
   is
      type Uint8_Array is array (Positive range <>) of Interfaces.Unsigned_8;
      Content : Uint8_Array (1 .. Count);
      for Content'Address use Bytes;
      pragma Import (Ada, Content);
   begin
      for I in Content'Range loop
         Bytes_IO.Write (File, Content (I));
      end loop;
      pragma Unreferenced (File);
   end Write_Bytes;

   procedure Write_Trace_File is new
      GNATcov_RTS.Traces.Output.Generic_Write_Trace_File (Bytes_IO.File_Type);

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String := Ada.Command_Line.Command_Name;
      Filename     : String := "";
      Exec_Date    : Ada.Calendar.Time := Ada.Calendar.Clock;
      User_Data    : String := "")
   is
      File : Bytes_IO.File_Type;
   begin
      if Filename = "" then
         Bytes_IO.Create (File, Name => Default_Trace_Filename);
      else
         Bytes_IO.Create (File, Name => Filename);
      end if;
      Write_Trace_File
        (File, Buffers, Program_Name, Format_Date (Exec_Date), User_Data);
      Bytes_IO.Close (File);
   end Write_Trace_File;

end GNATcov_RTS.Traces.Output.Files;
