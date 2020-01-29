--  This unit needs to be compilable with Ada 2005 compilers

with Ada.Text_IO;

with Interfaces;

with GNATcov_RTS.Buffers;       use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package body GNATcov_RTS.Traces.Generic_Output is

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
      Bytes   : Base64_Buffer_Array := (others => 0);
      Next    : Base64_Buffer_Index := 1;
      Columns : Natural := 0;
   end record;

   procedure Write_Bytes
     (Output : in out Base64_Buffer; Bytes : System.Address; Count : Natural);
   --  Callback for Generic_Write_Trace_File

   procedure Flush (Output : in out Base64_Buffer);
   --  Flush the remaining bytes in Output to the standard output. If the
   --  buffer is not full, this means it's the end of the content: pad with
   --  '=' bytes as needed.

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

      Alignment : constant Any_Endianity := System.Address'Size / 8;
      --  Alignment to use when writing trace files

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
               Bytes : constant String (1 .. Pad_count) :=
                  (others => ASCII.NUL);
            begin
               Write_Bytes (Output, Bytes'Address, Pad_Count);
            end;
         end if;
      end Write_Padding;

      ------------------
      -- Write_Header --
      ------------------

      procedure Write_Header (Output : in out Output_Stream) is
         Header : constant Trace_File_Header :=
           (Magic          => Trace_File_Magic,
            Format_Version => Current_Version,
            Alignment      => Any_Alignment (Alignment),
            Endianity      => Native_Endianity,
            Padding        => 0);
      begin
         Write_Bytes (Output, Header'Address, Header'Size / 8);
      end Write_Header;

      ----------------
      -- Write_Info --
      ----------------

      procedure Write_Info
        (Output : in out Output_Stream;
         Kind   : Supported_Info_Kind;
         Data   : String)
      is
         Header : constant Trace_Info_Header :=
           (Kind   => Kind,
            Length => Data'Length);
      begin
         Write_Bytes (Output, Header'Address, Header'Size / 8);
         Write_Bytes (Output, Data'Address, Data'Length);
         Write_Padding (Output, Data'Length);
      end Write_Info;

      -----------------
      -- Write_Entry --
      -----------------

      procedure Write_Entry
        (Output : in out Output_Stream; Buffers : Unit_Coverage_Buffers)
      is
         Header : constant Trace_Entry_Header :=
           (Unit_Name_Length    =>
               Interfaces.Unsigned_32 (Buffers.Unit_Name_Length),
            Statement_Bit_Count =>
               Traces.Any_Bit_Count (Buffers.Statement_Last_Bit + 1),
            Decision_Bit_Count  =>
               Traces.Any_Bit_Count (Buffers.Decision_Last_Bit + 1),
            MCDC_Bit_Count      =>
               Traces.Any_Bit_Count (Buffers.MCDC_Last_Bit + 1),
            Unit_Part           => Unit_Part_Map (Buffers.Unit_Part),
            Bit_Buffer_Encoding => LSB_First_Bytes,
            Fingerprint         => Buffers.Fingerprint,
            Padding             => (others => ASCII.NUL));
      begin
         Write_Bytes (Output, Header'Address, Header'Size / 8);
         Write_Bytes (Output, Buffers.Unit_Name'Address,
                      Buffers.Unit_Name'Length);
         Write_Padding (Output, Buffers.Unit_Name'Length);
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
            use type Interfaces.Unsigned_8;
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
               Write_Bytes (Output, Current_Byte'Address, 1);
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
         Buffer : constant Coverage_Buffer_Type (0 .. Last_Bit);
         for Buffer'Address use Buffer_Address;
         pragma Import (Ada, Buffer);
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

      function "+" (Bits : Uint6) return Character is (Base64_Alphabet (Bits));

      --  Split In_Bytes (3 bytes = 24 bits) into 4 groups of 6 bits

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
            Out_Digits (4) := +(Uint6'Mod (In_Bytes (3)));
      end case;

      --  Output the 4 characters corresponding to each group of 6 bits.
      --  Introduce a newline when needed in order to avoid exceeding 80
      --  characters per line.

      Ada.Text_IO.Put (Out_Digits);
      Output.Columns := Output.Columns + 4;
      if Output.Columns >= 80 then
         Output.Columns := 0;
         Ada.Text_IO.New_Line;
      end if;

      Output.Bytes := (others => 0);
      Output.Next := 1;
   end Flush;

   -----------------------------
   -- Write_Trace_File_Base64 --
   -----------------------------

   procedure Write_Trace_File_Base64
     (Buffers      : Unit_Coverage_Buffers_Array;
      Program_Name : String;
      Exec_Date    : Serialized_Timestamp;
      User_Data    : String := "")
   is
      procedure Helper is new Generic_Write_Trace_File (Base64_Buffer);
      Buffer : Base64_Buffer := (others => <>);
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("== GNATcoverage source trace file ==");
      Helper (Buffer, Buffers, Program_Name, Exec_Date, User_Data);
      Flush (Buffer);
      if Buffer.Columns /= 0 then
         Ada.Text_IO.New_Line;
      end if;
      Ada.Text_IO.Put_Line ("== End ==");
      Ada.Text_IO.New_Line;
   end Write_Trace_File_Base64;

end GNATcov_RTS.Traces.Generic_Output;
