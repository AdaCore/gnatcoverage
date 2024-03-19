------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with Ada.Unchecked_Conversion;

with System;

with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;

package body GNATcov_RTS.Traces.Output is

   ------------------------------
   -- Generic_Write_Trace_File --
   ------------------------------

   procedure Generic_Write_Trace_File
     (Output       : in out Output_Stream;
      Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String;
      Exec_Date    : Serialized_Timestamp;
      User_Data    : String := "")
   is

      Alignment : constant Unsigned_8 := 8;
      --  Alignment (in bytes) to use when writing trace files. It is used
      --  to align record types / strings written in the trace.

      generic
         type T is mod <>;
         with function Shift_Right
           (Value  : T;
            Amount : Natural) return T is <>;
      procedure Write_Unsigned (Output : in out Output_Stream; U : T);
      --  Generic implementation for Write_U8, Write_U16 and Write_U32.
      --  Write the bytes in little-endian order.

      procedure Write_Padding (Output : in out Output_Stream; Count : Natural);
      --  Write X bytes of padding to Output so that Count + X is a multiple of
      --  Alignment.

      procedure Write_Header (Output : in out Output_Stream);
      --  Write a trace file header to Output

      procedure Write_Info
        (Output : in out Output_Stream;
         Kind   : Supported_Info_Kind;
         Data   : String);
      --  Write a trace info entry to Output

      procedure Write_Entry
        (Output : in out Output_Stream; Buffers : Unit_Coverage_Buffers);
      --  Write a whole trace entry (header, unit name and buffers) for Buffers
      --  to Output.

      procedure Write_Buffer
        (Output : in out Output_Stream; Buffer : Coverage_Buffer_Type);
      --  Write the Buffer coverage buffer to Output

      procedure Write_Buffer
        (Output         : in out Output_Stream;
         Buffer_Address : System.Address;
         Last_Bit       : Any_Bit_Id);
      --  Wrapper for Write_Buffer to use a buffer from its address and upper
      --  bound.

      --------------------
      -- Write_Unsigned --
      --------------------

      procedure Write_Unsigned (Output : in out Output_Stream; U : T) is
         Bytes : Uint8_Array (1 .. T'Size / 8);
         Value : T := U;
      begin
         for I in Bytes'Range loop
            Bytes (I) := Unsigned_8 (Value and 16#FF#);
            Value := Shift_Right (Value, 8);
         end loop;
         Write_Bytes (Output, Bytes);
      end Write_Unsigned;

      procedure Write_U8 is new Write_Unsigned (T => Unsigned_8);
      procedure Write_U16 is new Write_Unsigned (T => Unsigned_16);
      procedure Write_U32 is new Write_Unsigned (T => Unsigned_32);

      -------------------
      -- Write_Padding --
      -------------------

      procedure Write_Padding (Output : in out Output_Stream; Count : Natural)
      is
         Alignment : constant Natural :=
            Natural (Generic_Write_Trace_File.Alignment);
         Pad_Count : constant Natural := Alignment - Count mod Alignment;
      begin
         if Pad_Count /= Alignment then
            declare
               Bytes : constant Uint8_Array (1 .. Pad_Count) := (others => 0);
            begin
               Write_Bytes (Output, Bytes);
            end;
         end if;
      end Write_Padding;

      ------------------
      -- Write_Header --
      ------------------

      procedure Write_Header (Output : in out Output_Stream) is
      begin
         Write_Bytes (Output, From_String (Trace_File_Magic));
         Write_U32 (Output, Unsigned_32 (Current_Version));
         Write_U8 (Output, Alignment);

         --  The Write_Unsigned subprogram (which is used to write bytes)
         --  writes the bytes in the little endian order.

         Write_U8 (Output, Unsigned_8 (Little_Endian));
         Write_U16 (Output, 0);
      end Write_Header;

      ----------------
      -- Write_Info --
      ----------------

      procedure Write_Info
        (Output : in out Output_Stream;
         Kind   : Supported_Info_Kind;
         Data   : String)
      is
      begin
         Write_U32 (Output, Unsigned_32 (Kind));
         Write_U32 (Output, Unsigned_32 (Data'Length));
         Write_Bytes (Output, From_String (Data));
         Write_Padding (Output, Data'Length);
      end Write_Info;

      -----------------
      -- Write_Entry --
      -----------------

      procedure Write_Entry
        (Output : in out Output_Stream; Buffers : Unit_Coverage_Buffers)
      is
         type Unit_Name_String is new String (1 .. Buffers.Unit_Name_Length);
         type Unit_Name_String_Acc is access all Unit_Name_String;

         type Project_Name_String is
           new String (1 .. Buffers.Project_Name_Length);
         type Project_Name_String_Acc is access all Project_Name_String;

         function To_Unit_Name_String_Acc is
           new Ada.Unchecked_Conversion (System.Address, Unit_Name_String_Acc);

         function To_Project_Name_String_Acc is
           new Ada.Unchecked_Conversion
             (System.Address, Project_Name_String_Acc);

         Unit_Name : constant Unit_Name_String_Acc :=
           To_Unit_Name_String_Acc (Buffers.Unit_Name);

         Project_Name : constant Project_Name_String_Acc :=
           To_Project_Name_String_Acc (Buffers.Project_Name);

      begin
         --  Write trace entry header

         Write_U32 (Output, Unsigned_32 (Buffers.Unit_Name_Length));
         Write_U32 (Output, Unsigned_32 (Buffers.Project_Name_Length));
         Write_U32 (Output, Unsigned_32 (Buffers.Statement_Last_Bit + 1));
         Write_U32 (Output, Unsigned_32 (Buffers.Decision_Last_Bit + 1));
         Write_U32 (Output, Unsigned_32 (Buffers.MCDC_Last_Bit + 1));
         Write_U8
           (Output,
            Unsigned_8 (Any_Language_Kind_Map (Buffers.Language_Kind)));
         Write_U8
           (Output,
            Unsigned_8
              (GNATcov_RTS.Buffers.Any_Unit_Part'Pos (Buffers.Unit_Part)));
         Write_U8 (Output, Unsigned_8 (LSB_First_Bytes));
         Write_Bytes (Output, Uint8_Array (Buffers.Fingerprint));
         Write_Bytes (Output, (1 .. 5 => 0));

         --  Write unit name and project name

         Write_Bytes (Output, From_String (String (Unit_Name.all)));
         Write_Padding (Output, Unit_Name.all'Length);

         Write_Bytes (Output, From_String (String (Project_Name.all)));
         Write_Padding (Output, Project_Name.all'Length);

         --  Write coverage buffers

         Write_Buffer (Output, Buffers.Statement, Buffers.Statement_Last_Bit);
         Write_Buffer (Output, Buffers.Decision, Buffers.Decision_Last_Bit);
         Write_Buffer (Output, Buffers.MCDC, Buffers.MCDC_Last_Bit);
      end Write_Entry;

      ------------------
      -- Write_Buffer --
      ------------------

      procedure Write_Buffer
        (Output : in out Output_Stream; Buffer : Coverage_Buffer_Type)
      is

         procedure Append_Bit (Value : Boolean);
         procedure Flush;

         Current_Byte : Interfaces.Unsigned_8 := 0;
         Bit_Mask     : Interfaces.Unsigned_8 := 1;
         Bytes_Count  : Natural := 0;

         ----------------
         -- Append_Bit --
         ----------------

         procedure Append_Bit (Value : Boolean) is
         begin
            if Value then
               Current_Byte := Current_Byte or Bit_Mask;
            end if;
            Bit_Mask := 2 * Bit_Mask;
            if Bit_Mask = 2 ** 8 then
               Flush;
            end if;
         end Append_Bit;

         -----------
         -- Flush --
         -----------

         procedure Flush is
         begin
            if Bit_Mask /= 1 then
               Write_Bytes (Output, (1 => Current_Byte));
               Current_Byte := 0;
               Bit_Mask := 1;
               Bytes_Count := Bytes_Count + 1;
            end if;
         end Flush;

      --  Start of processing for Write_Buffer

      begin
         --  Write bytes that are included in the coverage buffer

         for J in Buffer'Range loop
            Append_Bit (Buffer (J));
         end loop;
         Flush;

         --  Complete with the required padding

         Write_Padding (Output, Bytes_Count);
      end Write_Buffer;

      ------------------
      -- Write_Buffer --
      ------------------

      procedure Write_Buffer
        (Output         : in out Output_Stream;
         Buffer_Address : System.Address;
         Last_Bit       : Any_Bit_Id)
      is
         Buffer : Coverage_Buffer_Type (0 .. Last_Bit);
         for Buffer'Address use Buffer_Address;
         pragma Import (Ada, Buffer);

         --  Buffer could be declared constant in theory, but would then be
         --  wrongly flagged as an incorrectly placed deferred constant
         --  declaration by some old toolchains (as observed with 5.04).
      begin
         Write_Buffer (Output, Buffer);
      end Write_Buffer;

   --  Start of processing for Generic_Write_Trace_File

   begin
      Write_Header (Output);
      Write_Info (Output, Info_Program_Name, Program_Name);
      Write_Info (Output, Info_Exec_Date, Exec_Date);
      Write_Info (Output, Info_User_Data, User_Data);
      Write_Info (Output, Info_End, "");
      for I in Buffers'Range loop
         Write_Entry (Output, Buffers (I).all);
      end loop;
   end Generic_Write_Trace_File;

end GNATcov_RTS.Traces.Output;
