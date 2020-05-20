--  This unit needs to be compilable with Ada 95 compilers

with Interfaces;

with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

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

end GNATcov_RTS.Traces.Output;
