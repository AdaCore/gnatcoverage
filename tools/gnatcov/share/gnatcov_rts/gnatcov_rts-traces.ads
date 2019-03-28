--  Trace file format for instrumented programs

--  This unit needs to be compilable with Ada 95 compilers

with Interfaces; use Interfaces;

with GNATcov_RTS.Buffers;

package GNATcov_RTS.Traces is

   --  Execution of an instrumented program sets bits in its coverage buffers.
   --  These bits convey information that GNATcov will later on use to
   --  discharge coverage obligations.
   --
   --  This unit describes a file format for an instrumented program to
   --  communicate these coverage buffers to GNATcov.

   --  This format expects bytes/Characters to be 8-bit values.

   --  Each trace file starts with a header (see Trace_File_Header) and then
   --  contains zero or several trace entries, typically one per instrumented
   --  unit. Each trace entry is padded so that trace entries start and end on
   --  byte/half word/word/long word boundaries.

   type Trace_File_Format_Version is new Unsigned_32;
   Current_Version : Trace_File_Format_Version := 0;
   --  Expected value of the Trace_File_Header.Format_Version field.
   --
   --  TODO??? Bump to 1 once this feature is considered stable.

   type Any_Alignment is new Unsigned_8;
   subtype Supported_Alignment is Any_Alignment;
   --  Number of bytes the trace file writer used to pad trace entries. The
   --  size of the trace file is supposed to be a multiple of this value.
   --  Supported alignments are: 1, 2, 4 and 8.

   type Any_Endianity is new Unsigned_8;
   Little_Endian : constant Any_Endianity := 0;
   Big_Endian    : constant Any_Endianity := 1;
   subtype Supported_Endianity is
      Any_Endianity range Little_Endian ..  Big_Endian;
   --  Endianity to decode multi-byte scalars

   function Native_Endianity return Supported_Endianity;
   --  Return the native endianity

   type Hash_Type is new Unsigned_32;
   --  Hash type to perform consistency checks

   type Any_Unit_Part is new Unsigned_8;
   Unit_Body     : constant Any_Unit_Part :=
      Buffers.Any_Unit_Part'Pos (Buffers.Unit_Body);
   Unit_Spec     : constant Any_Unit_Part :=
      Buffers.Any_Unit_Part'Pos (Buffers.Unit_Spec);
   Unit_Separate : constant Any_Unit_Part :=
      Buffers.Any_Unit_Part'Pos (Buffers.Unit_Separate);
   subtype Supported_Unit_Part is
      Any_Unit_Part range Unit_Body .. Unit_Separate;
   --  Describe the kind of unit referenced by a trace entry

   Unit_Part_Map : constant
      array (Buffers.Any_Unit_Part) of Supported_Unit_Part :=
     (Buffers.Unit_Body     => Unit_Body,
      Buffers.Unit_Spec     => Unit_Spec,
      Buffers.Unit_Separate => Unit_Separate);

   type Any_Bit_Count is new Unsigned_32;
   --  Number of bits contained in a coverage buffer

   -----------------------
   -- Trace file header --
   -----------------------

   Trace_File_Magic : constant String (1 .. 32) :=
      "GNATcov source trace file" & (26 .. 32 => ASCII.NUL);
   --  Expected value of the Trace_File_Header.Magic field (32 bytes)

   type Trace_File_Header is record
      Magic : String (1 .. 32) := Trace_File_Magic;

      Format_Version : Trace_File_Format_Version;
      Alignment      : Any_Alignment;
      Endianity      : Any_Endianity;

      Padding : Unsigned_16;
      --  Padding used only to make the size of the trace file header a
      --  multiple of 8 bytes. Must be zero.
   end record;

   for Trace_File_Header use record
      Magic          at  0 range 0 .. 32 * 8 - 1;
      Format_Version at 32 range 0 .. 31;
      Alignment      at 36 range 0 .. 7;
      Endianity      at 37 range 0 .. 7;
      Padding        at 38 range 0 .. 15;
   end record;

   for Trace_File_Header'Size use 40 * 8;

   ------------------------
   -- Trace entry header --
   ------------------------

   --  Each trace entry starts with the following header. Then goes:
   --
   --  * The name of the unit describes. It is NUL-padded according to the
   --    trace file alignment.
   --
   --  * The statement coverage buffer. It is also NUL-padded.
   --
   --  * The decision coverage buffer. It is also NUL-padded.
   --
   --  * The MC/DC coverage buffer. It is also NUL-padded.

   type Any_Bit_Buffer_Encoding is new Unsigned_8;
   --  Encoding used to store coverage buffers

   LSB_First_Bytes : constant Any_Bit_Buffer_Encoding := 0;
   --  LSB_First_Bytes: bit buffers are encoded as sequences of bytes.
   --
   --  * Booleans are encoded with bits the usual way: 0 for False and 1 for
   --    True.
   --
   --  * Byte X contains bits for bits 8 * Y to 8 * Y + 7. For instance, byte
   --    1 contains bits 8 to 15.
   --
   --  * Inside byte X, least significant bit maps to bit 8 * Y while the most
   --    significant bit maps to bit 8 * Y + 7.

   subtype Supported_Bit_Buffer_Encoding is
      Any_Bit_Buffer_Encoding range LSB_First_Bytes .. LSB_First_Bytes;

   type Trace_Entry_Header is record
      Closure_Hash : Hash_Type;
      --  Hash for the instrumented unit and its complete dependency closure.
      --  This hash is used as a fast way to check that coverage obligations
      --  and coverage data are consistent.

      Unit_Name_Length : Unsigned_32;
      --  Length of the unit name for the unit this trace entry describes

      Statement_Bit_Count : Any_Bit_Count;
      Decision_Bit_Count  : Any_Bit_Count;
      MCDC_Bit_Count      : Any_Bit_Count;
      --  Number of bits in the statement, decision and MC/DC coverage buffers

      Unit_Part : Any_Unit_Part;
      --  Part of the unit this trace entry describes

      Bit_Buffer_Encoding : Any_Bit_Buffer_Encoding;
      --  Encoding used to represent statement and decision coverage buffers

      Padding : String (1 .. 2);
      --  Padding used only to make the size of this trace entry header a
      --  multiple of 8 bytes. Must be zero.
   end record;

   for Trace_Entry_Header use record
      Closure_Hash        at  0 range 0 .. 31;
      Unit_Name_Length    at  4 range 0 .. 31;
      Statement_Bit_Count at  8 range 0 .. 31;
      Decision_Bit_Count  at 12 range 0 .. 31;
      MCDC_Bit_Count      at 16 range 0 .. 31;
      Unit_Part           at 20 range 0 .. 7;
      Bit_Buffer_Encoding at 21 range 0 .. 7;
      Padding             at 22 range 0 .. 2 * 8 - 1;
   end record;

   for Trace_Entry_Header'Size use 24 * 8;

end GNATcov_RTS.Traces;
