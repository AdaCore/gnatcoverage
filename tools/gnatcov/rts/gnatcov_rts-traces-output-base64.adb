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

with GNATcov_RTS.Traces.Output.Text_IO;

package body GNATcov_RTS.Traces.Output.Base64 is

   package TIO renames Output.Text_IO;  --  Just a shorthand

   --  Base64-over-stdout stream

   type Uint6 is mod 2 ** 6;
   Base64_Alphabet : constant array (Uint6) of Character :=
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      & "abcdefghijklmnopqrstuvwxyz"
      & "0123456789"
      & "+/";
   Base64_Padding : constant Character := '=';

   type Base64_Buffer_Index is range 1 .. 4;
   subtype Valid_Base64_Buffer_Index is Base64_Buffer_Index range 1 .. 3;
   type Base64_Buffer_Array is array (Valid_Base64_Buffer_Index) of Unsigned_8;

   type Base64_Buffer is record
      Bytes : Base64_Buffer_Array := (others => 0);
      --  We output base64 content by groups of 4 digits, which encode for 3
      --  input bytes. This buffer is used to accumulate input bytes.

      Next : Base64_Buffer_Index := 1;
      --  Index of the next cell in Bytes where to store an input byte. This
      --  means that a given time, only the Bytes (1 .. Next - 1) slice contain
      --  input bytes.

      Columns : Natural := 0;
      --  Number of output digits on the current line. We keep track of this to
      --  break lines at 80 digits.
   end record;

   procedure Write_Bytes
     (Output : in out Base64_Buffer; Bytes : System.Address; Count : Natural);
   --  Callback for Generic_Write_Trace_File

   procedure Flush (Output : in out Base64_Buffer);
   --  Flush the remaining bytes in Output to the standard output. If the
   --  buffer is not full, this means it's the end of the content: pad with
   --  '=' bytes as needed.

   -----------------
   -- Write_Bytes --
   -----------------

   procedure Write_Bytes
     (Output : in out Base64_Buffer; Bytes : System.Address; Count : Natural)
   is
      Bytes_Array : array (1 .. Count) of Interfaces.Unsigned_8;
      for Bytes_Array'Address use Bytes;
      pragma Import (Ada, Bytes_Array);
   begin
      for I in Bytes_Array'Range loop
         Output.Bytes (Output.Next) := Bytes_Array (I);
         Output.Next := Output.Next + 1;
         if Output.Next = Base64_Buffer_Index'Last then
            Flush (Output);
         end if;
      end loop;
   end Write_Bytes;

   -----------
   -- Flush --
   -----------

   procedure Flush (Output : in out Base64_Buffer) is
      use Interfaces;

      function "+" (Bits : Uint6) return Character;
      --  Shortcut to get the ASCII representation of the given 6 bits

      ---------
      -- "+" --
      ---------

      function "+" (Bits : Uint6) return Character is
      begin
         return Base64_Alphabet (Bits);
      end "+";

      --  Split input bytes (3 bytes = 24 bits) into 4 groups of 6 bits

      In_Bytes   : Base64_Buffer_Array renames Output.Bytes;
      Out_Digits : String (1 .. 4);
   begin
      case Output.Next is
         when 1 =>
            return;

         when 2 =>
            Out_Digits (1) := +Uint6 (In_Bytes (1) / 4);
            Out_Digits (2) := +(Uint6 (In_Bytes (1) mod 4) * 16);
            Out_Digits (3) := Base64_Padding;
            Out_Digits (4) := Base64_Padding;

         when 3 =>
            Out_Digits (1) := +Uint6 (In_Bytes (1) / 4);
            Out_Digits (2) := +(Uint6 (In_Bytes (1) mod 4) * 16
                                or Uint6 (In_Bytes (2) / 16));
            Out_Digits (3) := +(Uint6 (In_Bytes (2) mod 16) * 4);
            Out_Digits (4) := Base64_Padding;

         when 4 =>
            Out_Digits (1) := +Uint6 (In_Bytes (1) / 4);
            Out_Digits (2) := +(Uint6 (In_Bytes (1) mod 4) * 16
                                or Uint6 (In_Bytes (2) / 16));
            Out_Digits (3) := +(Uint6 (In_Bytes (2) mod 16) * 4
                                or Uint6 (In_Bytes (3) / 64));
            Out_Digits (4) := +(Uint6 (In_Bytes (3) mod 64));
      end case;

      --  Output the 4 characters corresponding to each group of 6 bits.
      --  Introduce a newline when needed in order to avoid exceeding 80
      --  characters per line.

      TIO.Put (Out_Digits);
      Output.Columns := Output.Columns + 4;
      if Output.Columns >= 80 then
         Output.Columns := 0;
         TIO.New_Line;
      end if;

      Output.Bytes := (others => 0);
      Output.Next := 1;
   end Flush;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String;
      Exec_Date    : Serialized_Timestamp;
      User_Data    : String := "")
   is
      procedure Helper is new Generic_Write_Trace_File (Base64_Buffer);
      Buffer : Base64_Buffer :=
        (Bytes   => (others => 0),
         Next    => 1,
         Columns => 0);
   begin
      TIO.New_Line;
      TIO.Put_Line ("== GNATcoverage source trace file ==");
      Helper (Buffer, Buffers, Program_Name, Exec_Date, User_Data);
      Flush (Buffer);
      if Buffer.Columns /= 0 then
         TIO.New_Line;
      end if;
      TIO.Put_Line ("== End ==");
      TIO.New_Line;
   end Write_Trace_File;

end GNATcov_RTS.Traces.Output.Base64;
