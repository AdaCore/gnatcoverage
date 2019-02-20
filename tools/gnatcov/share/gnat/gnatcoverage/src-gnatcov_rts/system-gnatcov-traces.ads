--  Trace file format for instrumented programs

with Interfaces; use Interfaces;

package System.GNATcov.Traces is

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
   subtype Supported_Alignment is Any_Alignment
      with Static_Predicate => Supported_Alignment in 1 | 2 | 4 | 8;
   --  Number of bytes the trace file writer used to pad trace entries. The
   --  size of the trace file is supposed to be a multiple of this value.

   type Any_Endianity is new Unsigned_8;
   Little_Endian : constant Any_Endianity := 0;
   Big_Endian    : constant Any_Endianity := 1;
   subtype Supported_Endianity is Any_Endianity
      with Static_Predicate =>
         Supported_Endianity in Little_Endian | Big_Endian;
   --  Endianity to decode multi-byte scalars

   function Native_Endianity return Supported_Endianity is
     (if Default_Bit_Order = Low_Order_First
      then Little_Endian
      else Big_Endian);
   --  Return the native endianity

   type Hash_Type is new Unsigned_32;
   --  Hash type to perform consistency checks

   type Any_Unit_Kind is new Unsigned_8;
   Unit_Spec : constant Any_Unit_Kind := 0;
   Unit_Body : constant Any_Unit_Kind := 1;
   subtype Supported_Unit_Kind is Any_Unit_Kind
      with Static_Predicate => Supported_Unit_Kind in Unit_Spec | Unit_Body;
   --  Describe the kind of unit referenced by a trace entry

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
   --  Bit buffers have a simple encoding. They are sequences of bytes.
   --
   --  * Booleans are encoded with bits the usual way: 0 for False and 1 for
   --    True.
   --
   --  * Byte X contains bits for bits 8 * Y to 8 * Y + 7. For instance, byte
   --    1 contains bits 8 to 15.
   --
   --  * Inside byte X, least significant bit maps to bit 8 * Y while the most
   --    significant bit maps to bit 8 * Y + 7.

   type Trace_Entry_Header is record
      Closure_Hash : Hash_Type;
      --  Hash for the instrumented unit and its complete dependency closure.
      --  This hash is used as a fast way to check that coverage obligations
      --  and coverage data are consistent.

      Unit_Name_Length : Unsigned_32;
      --  Length of the unit name for the unit this trace entry describes

      Stmt_Bit_Count : Any_Bit_Count;
      Dc_Bit_Count   : Any_Bit_Count;
      --  Number of bits in the statement and decision coverage buffers

      Unit_Kind : Any_Unit_Kind;
      --  Kind for the unit this trace entry describes

      Padding : String (1 .. 7);
      --  Padding used only to make the size of this trace entry header a
      --  multiple of 8 bytes. Must be zero.
   end record;

   for Trace_Entry_Header use record
      Closure_Hash     at  0 range 0 .. 31;
      Unit_Name_Length at  4 range 0 .. 31;
      Stmt_Bit_Count   at  8 range 0 .. 31;
      Dc_Bit_Count     at 12 range 0 .. 31;
      Unit_Kind        at 16 range 0 .. 7;
      Padding          at 17 range 0 .. 7 * 8 - 1;
   end record;

   for Trace_Entry_Header'Size use 24 * 8;

end System.GNATcov.Traces;
