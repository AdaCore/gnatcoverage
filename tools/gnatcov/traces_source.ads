------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with Interfaces; use Interfaces;

with GPR2;

--  Describes the content of a source trace file. Should be kept up to date
--  when the trace format is modified. It is notably used to parse source
--  traces in gnatcov.

package Traces_Source is

   --  Execution of an instrumented program sets bits in its coverage buffers.
   --  These bits convey information that GNATcov will later on use to
   --  discharge coverage obligations.
   --
   --  This unit describes a file format for an instrumented program to
   --  communicate these coverage buffers to GNATcov.

   --  This format expects bytes/Characters to be 8-bit values.

   --  Each trace file starts with a header (see Trace_File_Header), followed
   --  by a sequence of trace info entries (see Trace_Entry_Header) ending with
   --  a Info_End entry, and then contains zero or several trace entries,
   --  typically one per instrumented unit. Each trace entry is padded so that
   --  trace entries start and end on byte/half word/word/long word boundaries.

   type Trace_File_Format_Version is new Unsigned_32;
   Current_Version : Trace_File_Format_Version := 4;
   --  Expected value of the Trace_File_Header.Format_Version field.
   --
   --  0 -- initial version
   --  1 -- extend trace entry model to account for C files
   --  2 -- introduce fingerprints for bit maps
   --  3 -- remove the project name from trace entries
   --  4 -- introduce fingerprint for annotations

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
   Not_Applicable_Part : constant Any_Unit_Part := 0;
   Unit_Body           : constant Any_Unit_Part := 1;
   Unit_Spec           : constant Any_Unit_Part := 2;
   Unit_Separate       : constant Any_Unit_Part := 3;
   subtype All_Unit_Part is
      Any_Unit_Part range Not_Applicable_Part .. Unit_Separate;
   subtype Supported_Unit_Part is
      Any_Unit_Part range Unit_Body .. Unit_Separate;
   --  Describe the kind of unit referenced by a trace entry

   function "+"
     (Part : Supported_Unit_Part) return GPR2.Valid_Unit_Kind
   is (case Part is
       when Unit_Body     => GPR2.S_Body,
       when Unit_Spec     => GPR2.S_Spec,
       when Unit_Separate => GPR2.S_Separate);
   function "+"
     (Part : GPR2.Valid_Unit_Kind) return Supported_Unit_Part
   is (case Part is
       when GPR2.S_Body     => Unit_Body,
       when GPR2.S_Spec     => Unit_Spec,
       when GPR2.S_Separate => Unit_Separate);

   type Any_Bit_Count is new Unsigned_32;
   --  Number of bits contained in a coverage buffer

   type Any_Language_Kind is new Unsigned_8;
   Unit_Based_Language : constant Any_Language_Kind := 0;
   File_Based_Language : constant Any_Language_Kind := 1;
   subtype Supported_Language_Kind is
     Any_Language_Kind range Unit_Based_Language .. File_Based_Language;
   --  Language kind for a compilation unit

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

   -----------------------
   -- Trace information --
   -----------------------

   --  Each trace info entry starts with the following header. Then goes the
   --  bytes for the entry content, NUL-padded according to the trace file
   --  alignment.

   type Any_Info_Kind is new Unsigned_32;

   Info_End : constant Any_Info_Kind := 0;
   --  Special trace info entry: indicates the end of a sequence of entries. No
   --  data is associated to this trace info entry.

   Info_Program_Name : constant Any_Info_Kind := 1;
   --  Name of the program that produced this trace

   Info_Exec_Date : constant Any_Info_Kind := 2;
   --  Date for the program execution that produced this trace

   Info_User_Data : constant Any_Info_Kind := 3;
   --  Arbitrary storage for user data. This is exposed to users as the trace
   --  "tag".

   subtype Supported_Info_Kind is
      Any_Info_Kind range Info_End ..  Info_User_Data;

   type Trace_Info_Header is record
      Kind : Any_Info_Kind;
      --  Kind for this trace info entry

      Length : Unsigned_32;
      --  Length of the data associated to this entry
   end record;

   for Trace_Info_Header use record
      Kind   at 0 range 0 .. 31;
      Length at 4 range 0 .. 31;
   end record;

   for Trace_Info_Header'Size use 8 * 8;

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

   type Fingerprint_Type is new String (1 .. 20);

   type Trace_Entry_Header is record
      Unit_Name_Length : Unsigned_32;
      --  Length of the unit name / filename for the unit this trace entry
      --  describes.

      Statement_Bit_Count : Any_Bit_Count;
      Decision_Bit_Count  : Any_Bit_Count;
      MCDC_Bit_Count      : Any_Bit_Count;
      --  Number of bits in the statement, decision and MC/DC coverage buffers

      Language_Kind : Any_Language_Kind;
      --  Language for this unit

      Unit_Part : Any_Unit_Part;
      --  Part of the unit this trace entry describes. Not_Applicable_Part for
      --  file-based languages.

      Bit_Buffer_Encoding : Any_Bit_Buffer_Encoding;
      --  Encoding used to represent statement and decision coverage buffers

      Fingerprint : Fingerprint_Type;
      --  Hash of SCO info for this unit. Useds a fast way to check that
      --  coverage obligations and coverage data are consistent. Specific hash
      --  values are computed during instrumentation.

      Bit_Maps_Fingerprint : Fingerprint_Type;
      --  Hash of buffer bit mappings for this unit, as gnatcov computes it
      --  (see SC_Obligations).  Used as a fast way to check that gnatcov will
      --  be able to interpret buffer bits from a source traces using buffer
      --  bit mappings from SID files.

      Annotations_Fingerprint : Fingerprint_Type;
      --  Hash of annotations for this unit, as gnatcov computes it (see
      --  SC_Obligations). Used as a fast way to check that source traces and
      --  coverage data are consistent.

      Padding : String (1 .. 1);
      --  Padding used only to make the size of this trace entry header a
      --  multiple of 8 bytes. Must be zero.
   end record;

   for Trace_Entry_Header use record
      Unit_Name_Length        at  0 range 0 .. 31;
      Statement_Bit_Count     at  4 range 0 .. 31;
      Decision_Bit_Count      at  8 range 0 .. 31;
      MCDC_Bit_Count          at 12 range 0 .. 31;
      Language_Kind           at 16 range 0 .. 7;
      Unit_Part               at 17 range 0 .. 7;
      Bit_Buffer_Encoding     at 18 range 0 .. 7;
      Fingerprint             at 19 range 0 .. 20 * 8 - 1;
      Bit_Maps_Fingerprint    at 39 range 0 .. 20 * 8 - 1;
      Annotations_Fingerprint at 59 range 0 .. 20 * 8 - 1;
      Padding                 at 79 range 0 .. 1 * 8 - 1;
   end record;

   for Trace_Entry_Header'Size use 80 * 8;

end Traces_Source;
