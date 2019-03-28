--  This unit needs to be compilable with Ada 2005 compilers

with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Direct_IO;
with Ada.Directories;

with Interfaces;

with System;

with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;

package body GNATcov_RTS.Traces.Output is

   package Bytes_IO is new Ada.Direct_IO (Interfaces.Unsigned_8);
   use Bytes_IO;

   function Alignment return Any_Endianity;
   --  Return the alignment to use when writing trace files

   function Trace_Filename (Filename : String) return String;
   --  Return the name of trace file to write to in Write_Trace_File

   procedure Write_Bytes
     (File : File_Type; Bytes : System.Address; Count : Natural);
   --  Write to File the content of the memory starting at Bytes and that
   --  contains Count bytes.

   procedure Write_Padding (File : File_Type; Count : Natural);
   --  Write X bytes of padding to File so that Count + X is a multiple of
   --  Alignment.

   procedure Write_Header (File : File_Type);
   --  Write a trace file header to File

   procedure Write_Entry (File : File_Type; Buffers : Unit_Coverage_Buffers);
   --  Write a whole trace entry (header, unit name and buffers) for Buffers to
   --  File.

   procedure Write_Buffer (File : File_Type; Buffer : Coverage_Buffer_Type);
   --  Write the Buffer coverage buffer to File

   procedure Write_Buffer
     (File           : File_Type;
      Buffer_Address : System.Address;
      Last_Bit       : Any_Bit_Id);
   --  Wrapper for Write_Buffer to use a buffer from its address and upper
   --  bound.

   ---------------
   -- Alignment --
   ---------------

   function Alignment return Any_Endianity is
   begin
      return System.Address'Size / 8;
   end Alignment;

   -----------------
   -- Write_Bytes --
   -----------------

   procedure Write_Bytes
     (File : File_Type; Bytes : System.Address; Count : Natural)
   is
      Array_View : array (1 .. Count) of Interfaces.Unsigned_8;
      for Array_View'Address use Bytes;
   begin
      for I in Array_View'Range loop
         Write (File, Array_View (I));
      end loop;
   end Write_Bytes;

   -------------------
   -- Write_Padding --
   -------------------

   procedure Write_Padding (File : File_Type; Count : Natural) is
      Alignment : constant Natural := Natural (Output.Alignment);
      Pad_Count : Natural := Alignment - Count mod Alignment;
   begin
      if Pad_Count /= Alignment then
         while Pad_Count > 0 loop
            Write (File, 0);
            Pad_Count := Pad_Count - 1;
         end loop;
      end if;
   end Write_Padding;

   ------------------
   -- Write_Header --
   ------------------

   procedure Write_Header (File : File_Type) is
      Header : constant Trace_File_Header :=
        (Magic          => Trace_File_Magic,
         Format_Version => Current_Version,
         Alignment      => Any_Alignment (Alignment),
         Endianity      => Native_Endianity,
         Padding        => 0);
   begin
      Write_Bytes (File, Header'Address, Header'Size / 8);
   end Write_Header;

   -----------------
   -- Write_Entry --
   -----------------

   procedure Write_Entry (File : File_Type; Buffers : Unit_Coverage_Buffers)
   is
      Header : constant Trace_Entry_Header :=
        (Closure_Hash        => Traces.Hash_Type (Buffers.Closure_Hash),
         Unit_Name_Length    =>
            Interfaces.Unsigned_32 (Buffers.Unit_Name_Length),
         Statement_Bit_Count =>
            Traces.Any_Bit_Count (Buffers.Statement_Last_Bit + 1),
         Decision_Bit_Count  =>
            Traces.Any_Bit_Count (Buffers.Decision_Last_Bit + 1),
         MCDC_Bit_Count      =>
            Traces.Any_Bit_Count (Buffers.MCDC_Last_Bit + 1),
         Unit_Part           => Unit_Part_Map (Buffers.Unit_Part),
         Bit_Buffer_Encoding => LSB_First_Bytes,
         Padding             => (others => ASCII.NUL));
   begin
      Write_Bytes (File, Header'Address, Header'Size / 8);
      Write_Bytes (File, Buffers.Unit_Name'Address, Buffers.Unit_Name'Length);
      Write_Padding (File, Buffers.Unit_Name'Length);
      Write_Buffer (File, Buffers.Statement, Buffers.Statement_Last_Bit);
      Write_Buffer (File, Buffers.Decision, Buffers.Decision_Last_Bit);
      Write_Buffer (File, Buffers.MCDC, Buffers.MCDC_Last_Bit);
   end Write_Entry;

   ------------------
   -- Write_Buffer --
   ------------------

   procedure Write_Buffer (File : File_Type; Buffer : Coverage_Buffer_Type) is
      Bytes_Count : Natural := Buffer'Size / 8;
      Extra_Bits  : constant Any_Bit_Id := Buffer'Size mod 8;
   begin
      --  Write bytes that are included in the coverage buffer

      Write_Bytes (File, Buffer'Address, Bytes_Count);

      --  Then write the remaining bits, padded with NUL bytes

      if Extra_Bits /= 0 then
         declare
            Byte : Interfaces.Unsigned_8 := 0;
         begin
            for I in Buffer'Last - Extra_Bits + 1 .. Buffer'Last loop
               Byte := 2 * Byte + Boolean'Pos (Buffer (I));
            end loop;
            Write (File, Byte);
         end;
         Bytes_Count := Bytes_Count + 1;
      end if;

      --  Complete with the required padding

      Write_Padding (File, Bytes_Count);
   end Write_Buffer;

   ------------------
   -- Write_Buffer --
   ------------------

   procedure Write_Buffer
     (File           : File_Type;
      Buffer_Address : System.Address;
      Last_Bit       : Any_Bit_Id)
   is
      Buffer : constant Coverage_Buffer_Type (0 .. Last_Bit);
      for Buffer'Address use Buffer_Address;
      pragma Import (Ada, Buffer);
   begin
      Write_Buffer (File, Buffer);
   end Write_Buffer;

   --------------------
   -- Trace_Filename --
   --------------------

   function Trace_Filename (Filename : String) return String is
      use Ada.Environment_Variables;
   begin
      if Filename /= "" then
         return Filename;

      elsif Value (GNATCOV_TRACE_FILE, "") /= "" then
         return Value (GNATCOV_TRACE_FILE);

      else
         return Ada.Directories.Simple_Name (Ada.Command_Line.Command_Name)
                & ".srctrace";
      end if;
   end Trace_Filename;

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Buffers  : Unit_Coverage_Buffers_Array;
      Filename : String := "")
   is
      File : File_Type;
   begin
      Create (File, Name => Trace_Filename (Filename));
      Write_Header (File);
      for I in Buffers'Range loop
         Write_Entry (File, Buffers (I).all);
      end loop;
      Close (File);
   end Write_Trace_File;

end GNATcov_RTS.Traces.Output;
