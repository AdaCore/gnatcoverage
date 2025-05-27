------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This unit controls the generation and processing of coverage state
--  checkpoint files for incremental coverage.

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Vectors;
with Ada.Streams;    use Ada.Streams;
with Interfaces;

with GNAT.Regexp;
with Types; use Types;

with Coverage;
with Files_Table;    use Files_Table;
with Slocs;          use Slocs;
with SC_Obligations; use SC_Obligations;
with Strings;        use Strings;
with Traces;         use Traces;
with Traces_Lines;   use Traces_Lines;
with Traces_Source;  use Traces_Source;

package Checkpoints is

   subtype Checkpoint_Version is Interfaces.Unsigned_32 range 1 .. 18;
   --  For compatibility with previous Gnatcov versions, the checkpoint
   --  file format is versioned.
   --
   --  1  -- initial version of checkpoint support
   --  2  -- support for source instrumentation
   --  3  -- support for dumping names for units of interest
   --  4  -- gnatcov's own Pragma_Id type
   --  5  -- one CU_Info per source file and revamped fingerprint computation
   --        and support for listing ignored source files with --dump-units-to
   --  6  -- Add the kind of trace (Source, Binary or mixed) in the checkpoint
   --  7  -- Add the "bits" for binary traces used to create the checkpoint
   --  8  -- Add macro expansion information
   --  9  -- Add the support for scoped metrics
   --  10 -- Add non instrumented SCOs sets
   --  11 -- fingerprints for buffer bit maps
   --  12 -- Extend Unit_List to distinguish homonym source files
   --  13 -- Extend Files_Table.File_Info to distinguish homonym source files
   --  14 -- Extend CU_Info to implement block coverage
   --  15 -- Increase size of Pragma_Id after addition of 255th pragma
   --  16 -- Extend Scope_Entity to include the Start/End_Sloc of the scope
   --  17 -- Add support for Fun_Call and Guarded Expression coverage
   --  18 -- Removed support for tags (separate coverage)
   --
   --  Note that we always use the last version when creating a checkpoint.
   --
   --  TODO for the next checkpoint version:
   --
   --  * Inline the "instrumented?" booleans in SCO_Descriptor
   --    (de)serialization. They are currently stored as separate "maps" in
   --    SIDs/checkpoints.

   type Checkpoint_Purpose is (Instrumentation, Consolidation);
   --  Purpose of checkpoint can be to provide:
   --    * Instrumentation: SCO information from instrumentation of source
   --      files (SID files);
   --    * Consolidation: a snapshot of an intermediate state in consolidated
   --      coverage (regular checkpoints).

   function Purpose_Name (Purpose : Checkpoint_Purpose) return String
   is (case Purpose is
       when Instrumentation => "Source Instrumentation Data (SID)",
       when Consolidation   => "checkpoint");
   --  Return a user-level name to designate a checkpoint created for the given
   --  Purpose.

   type Checkpoint_Relocations is private;

   procedure Allocate_SFI_Maps
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : Source_File_Index);
   procedure Allocate_CU_Id_Maps
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : CU_Id);
   procedure Allocate_Inst_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : Inst_Id);
   procedure Allocate_BDD_Node_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : BDD_Node_Id);
   procedure Allocate_SCO_Id_Map
     (Relocs      : in out Checkpoint_Relocations;
      First, Last : SCO_Id);
   --  Allocate the various tables in the checkpoint relocations.

   function Get_Simple_Name
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Valid_Source_File_Index) return Unbounded_String;
   --  Return the simple file name for CP_SFI. Using this function is necessary
   --  when CP_SFI is ignored, as it is not possible to call Remap_SFI on it.

   procedure Ignore_SFI
     (Relocs : in out Checkpoint_Relocations;
      CP_SFI : Source_File_Index);
   procedure Ignore_CU_Id
     (Relocs   : in out Checkpoint_Relocations;
      CP_CU_Id : CU_Id);
   --  Mark the source file index or compilation unit as ignored in the
   --  checkpoint relocation. Tying to remap that source file index or
   --  compilation unit will result in an error.

   procedure Ignore_SCO
     (Relocs    : in out Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id);
   --  Mark the SCO_Id as being removed in the checkpoint relocation.
   --  No information relative to this SCO should be loaded. In particular,
   --  trying to remap this SCO_Id will result in an error.

   function SFI_Ignored
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Source_File_Index) return Boolean;
   --  Return whether CP_SFI is ignored in the checkpoint or not.
   --  For convenience, No_Source_File is considered as never ignored.

   function CU_Id_Ignored
     (Relocs   : Checkpoint_Relocations;
      CP_CU_Id : CU_Id) return Boolean;
   --  Return whether CP_CU_Id is ignored in the checkpoint or not.
   --  For convenience, No_CU_Id is never considered ignored.

   function SCO_Ignored
     (Relocs    : Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id) return Boolean;
   --  Return whether CP_SCO_Id was marked as removed in this checkpoint.
   --  For convenience, No_SCO_Id is never considered ignored.

   procedure Set_SFI_Map
     (Relocs                 : in out Checkpoint_Relocations;
      Source_SFI, Target_SFI : Valid_Source_File_Index) with
     Pre => not SFI_Ignored (Relocs, Source_SFI);
   --  Associate Source_SFI to Target_SFI in the relocations map.
   --  Source_SFI must not have been previously marked as ignored with
   --  Ignore_SFI.

   procedure Set_SFI_Simple_Name
     (Relocs      : in out Checkpoint_Relocations;
      SFI         : Valid_Source_File_Index;
      Simple_Name : Unbounded_String);
   --  Assign a simple name to SFI

   procedure Set_CU_Id_Map
     (Relocs                     : in out Checkpoint_Relocations;
      Source_CU_Id, Target_CU_Id : Valid_CU_Id) with
     Pre => not CU_Id_Ignored (Relocs, Source_CU_Id);
   --  Associate Source_CU_ID to Target_CU_ID in the relocations map.
   --  Source_CU_Id must not have been previously marked as ignored with
   --  Ignore_CU_Id.

   procedure Set_Inst_Id_Map
     (Relocs                         : in out Checkpoint_Relocations;
      Source_Inst_Id, Target_Inst_Id : Valid_Inst_Id);
   --  Associate Source_Inst_Id to Target_Inst_Id in the relocations map

   procedure Set_BDD_Node_Id_Map
     (Relocs                                 : in out Checkpoint_Relocations;
      Source_BDD_Node_Id, Target_BDD_Node_Id : Valid_BDD_Node_Id);
   --  Associate Source_BDD_Node_Id to Target_BDD_Node_ID in the relocation map

   procedure Set_SCO_Id_Map
     (Relocs                       : in out Checkpoint_Relocations;
      Source_SCO_Id, Target_SCO_Id : Valid_SCO_Id) with
     Pre => not SCO_Ignored (Relocs, Source_SCO_Id);
   --  Associate Source_SCO_ID to Target_SCO_ID in the relocations map

   procedure Free (Relocs : in out Checkpoint_Relocations);
   --  Relase allocated maps in Relocs

   function Remap_SFI
     (Relocs : Checkpoint_Relocations;
      CP_SFI : Source_File_Index) return Source_File_Index with
     Pre => not SFI_Ignored (Relocs, CP_SFI);
  --  Remap one source file index.
  --
  --  If CP_SFI is No_Source_File then it's returned unchanged. If it is
  --  any other value, then it is remapped to the corresponding value in
  --  the current run.

   function Remap_CU_Id
     (Relocs   : Checkpoint_Relocations;
      CP_CU_Id : CU_Id) return CU_Id with
     Pre => not CU_Id_Ignored (Relocs, CP_CU_Id);
   --  Remap one CU_Id.
   --
   --  If CP_CU_Id is No_CU_Id then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run.

   function Remap_Inst_Id
     (Relocs     : Checkpoint_Relocations;
      CP_Inst_Id : Inst_Id) return Inst_Id;
   --  Remap one Inst_Id.
   --
   --  If CP_Inst_Id is No_Inst_Id then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run.

   function Remap_BDD_Node_Id
     (Relocs         : Checkpoint_Relocations;
      CP_BDD_Node_Id : BDD_Node_Id) return BDD_Node_Id;
   --  Remap one BDD_Node_Id.
   --
   --  If CP_BDD_Node_Id is No_BDD_Node_Id then it's returned unchanged. If it
   --  is any other value, then it is remapped to the corresponding value in
   --  the current run.

   function Remap_SCO_Id
     (Relocs    : Checkpoint_Relocations;
      CP_SCO_Id : SCO_Id) return SCO_Id with
     Pre => not SCO_Ignored (Relocs, CP_SCO_Id);
   --  Remap one SCO_Id.
   --
   --  If CP_SCO_Id is No_SCO_Id then it's returned unchanged. If it
   --  is any other value, then it is remapped to the corresponding value in
   --  the current run.

   procedure Remap_SFI
     (Relocs : Checkpoint_Relocations;
      CP_SFI : in out Source_File_Index) with
     Pre => not SFI_Ignored (Relocs, CP_SFI);
   --  Remap one source file index.
   --
   --  If CP_SFI is No_Source_File then it's returned unchanged. If it is
   --  any other value, then it is remapped to the corresponding value in
   --  the current run.

   --  A stream associated with global state shared across phases of a
   --  checkpoint load or save.

   procedure Remap_ALI_Annotations
     (Relocs             : Checkpoint_Relocations;
      CP_ALI_Annotations : in out ALI_Annotation_Maps.Map);
   --  Remap one source file index.

   --  A stream associated with global state shared across phases of a
   --  checkpoint load or save.

   type Stateful_Stream (Stream : access Root_Stream_Type'Class) is abstract
     new Root_Stream_Type with
      record
         Filename : Unbounded_String;
         --  Name of the checkpoint being written/read

         Purpose : Checkpoint_Purpose;
         --  Purpose for the checkpoint being written/read
      end record;

   procedure Read
     (Stream : in out Stateful_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   procedure Write
     (Stream : in out Stateful_Stream;
      Item   : Stream_Element_Array);
   --  Read/write from/to underlying stream

   function Purpose_Of
     (SS : Stateful_Stream'Class) return Checkpoint_Purpose
   is (SS.Purpose)
     with Inline;
   --  Shortcut to get the purpose of a stream that is known to be an instance
   --  of Stateful_Stream.

   --  Global state shared across phases of a checkpoint save

   type Checkpoint_Save_State is new Stateful_Stream with null record;

   --  Global state shared across phases of a checkpoint load

   type Checkpoint_Load_State is new Stateful_Stream with record
      Version : Checkpoint_Version;
      --  Format version for the checkpoint being read

      Relocations : Checkpoint_Relocations;

      Static_Decision_Evaluations : Static_Decision_Evaluation_Maps.Map;
   end record;

   procedure Checkpoint_Save
     (Filename : String;
      Context  : access Coverage.Context;
      Purpose  : Checkpoint_Purpose);
   --  Dump internal data structures to a checkpoint file

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure SID_Load
     (Filename             : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp);
   --  Load an SID file into internal data structures, ignoring files
   --  that match Ignored_Source_Files.

   procedure Checkpoint_Load (Filename : String);
   --  Load a checkpoint file into internal data structures

   --  Note on checkpoint version compatibility: our goal for (de)serialization
   --  code is to avoid unintentional unintentional changes to the
   --  checkpoint/SID file format. Consider for instance the following
   --  hypothetical deserialization code:
   --
   --    Levels : Coverage_Options.Levels_Type;
   --
   --    Levels :=
   --      (for Level in Coverage_Options.Coverage_Level =>
   --       CLS.Read_Boolean);
   --
   --  This reads N booleans from the checkpoints, N being the number of
   --  enumeration values defined for Coverage_Levels. If a patch adds a new
   --  enumeration value (creation of a new coverage level), then this code
   --  will suddenly read one more boolean from the checkpoint, silently
   --  breaking the checkpoint file format, whereas such changes mandate the
   --  creation of a new checkpoint version number (see above).
   --
   --  To avoid this kind of problem, we deserialize code like the above with
   --  more "manual" code:
   --
   --    CP_Levels : U8_Array (1 .. 8);
   --    Levels    : Levels_Type;
   --
   --    CLS.Read (CP_Levels);
   --    Levels :=
   --      (Insn     => Boolean'Val (CP_Levels (1)),
   --       Branch   => Boolean'Val (CP_Levels (2)),
   --       [...]
   --
   --  The above will not compile when adding a new coverage level, since the
   --  aggregate and Levels will not have the same length.

   ----------------------------------------------
   -- Helpers to implement checkpoints loading --
   ----------------------------------------------

   --  All the Read_* functions below read bytes from the checkpoint and decode
   --  specific data types. There is no overload (functions have different
   --  names) to avoid ambiguities when writing for instance:
   --
   --     My_Type'Val (CLS.Read_U8)
   --
   --  Also define generic procedures to read and decode containers (maps,
   --  multiway trees, sets, vectors). For these, we prefer procedures with OUT
   --  parameters rather than functions to avoid unecessary copies for the
   --  decoded values (functions are more convenient, so we keep them for small
   --  data types). Yet, in order to be able to instantiate Read procedures for
   --  containers, we also define several Read procedure that wrap the Read_*
   --  functions.

   type U8_Array is array (Positive range <>) of Interfaces.Unsigned_8;

   function Read_BDD_Node
     (Self : in out Checkpoint_Load_State) return BDD_Node_Id;
   function Read_Bit_Id
     (Self : in out Checkpoint_Load_State) return Any_Bit_Id;
   function Read_Boolean (Self : in out Checkpoint_Load_State) return Boolean;
   function Read_CU (Self : in out Checkpoint_Load_State) return CU_Id;
   function Read_Compilation_Unit
     (Self : in out Checkpoint_Load_State) return Compilation_Unit;
   function Read_Condition
     (Self : in out Checkpoint_Load_State) return Any_Condition_Index;
   function Read_Fingerprint
     (Self : in out Checkpoint_Load_State)
      return SC_Obligations.Fingerprint_Type;
   function Read_I32
     (Self : in out Checkpoint_Load_State) return Interfaces.Integer_32;
   function Read_Inst (Self : in out Checkpoint_Load_State) return Inst_Id;
   function Read_Integer (Self : in out Checkpoint_Load_State) return Integer;
   function Read_Language_Kind
     (Self : in out Checkpoint_Load_State) return Supported_Language_Kind;
   function Read_Line_State
     (Self : in out Checkpoint_Load_State) return Any_Line_State;
   function Read_Local_Source_Location
     (Self : in out Checkpoint_Load_State) return Local_Source_Location;
   function Read_Local_Source_Location_Range
     (Self : in out Checkpoint_Load_State) return Local_Source_Location_Range;
   function Read_PC (Self : in out Checkpoint_Load_State) return Pc_Type;
   function Read_SCO (Self : in out Checkpoint_Load_State) return SCO_Id;
   function Read_SFI
     (Self : in out Checkpoint_Load_State) return Source_File_Index;
   function Read_Source_Location
     (Self : in out Checkpoint_Load_State) return Source_Location;
   function Read_Source_Location_Range
     (Self : in out Checkpoint_Load_State) return Source_Location_Range;
   function Read_String (Self : in out Checkpoint_Load_State) return String;
   function Read_Tristate
     (Self : in out Checkpoint_Load_State) return Tristate;
   function Read_U8
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_8;
   function Read_U16
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_16;
   function Read_U32
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_32;
   function Read_U64
     (Self : in out Checkpoint_Load_State) return Interfaces.Unsigned_64;
   function Read_Unbounded_String
     (Self : in out Checkpoint_Load_State) return Unbounded_String;

   generic
      type Key_Type is private;
      type Element_Type is private;
      type Map_Type is private;

      with procedure Clear (Self : in out Map_Type);
      with procedure Insert
        (Self : in out Map_Type; Key : Key_Type; Element : Element_Type);

      with procedure Read_Key
        (Self : in out Checkpoint_Load_State; Key : out Key_Type);
      with procedure Read_Element
        (Self : in out Checkpoint_Load_State; Element : out Element_Type);
   procedure Read_Map
     (Self : in out Checkpoint_Load_State; Map : out Map_Type);
   --  Generic implementation to read a map from a checkpoint. Since there are
   --  two flavors for maps (ordered and hashed), do not take a instantiated
   --  formal package, but rather the only two map primitives that we need:
   --  Clear and Insert.

   generic
      type Element_Type is private;
      type Set_Type is private;

      with procedure Clear (Self : in out Set_Type);
      with procedure Insert (Self : in out Set_Type; Element : Element_Type);

      with procedure Read_Element
        (Self : in out Checkpoint_Load_State; Element : out Element_Type);
   procedure Read_Set
     (Self : in out Checkpoint_Load_State; Set : out Set_Type);
   --  Generic implementation to read a set from a checkpoint. Since there are
   --  two flavors for sets (ordered and hashed), do not take a instantiated
   --  formal package, but rather the only two set primitives that we need:
   --  Clear and Insert.

   generic
      type Element_Type is private;
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
      with package Multiway_Trees is new Ada.Containers.Multiway_Trees
        (Element_Type, "=");
      with procedure Read_Element
        (Self : in out Checkpoint_Load_State; Element : out Element_Type);
   procedure Read_Tree
     (Self : in out Checkpoint_Load_State; Tree : out Multiway_Trees.Tree);
   --  Generic implementation to read a multiway tree from a checkpoint

   generic
      type Index_Type is range <>;
      type Element_Type is private;
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
      with package Vectors is new Ada.Containers.Vectors
        (Index_Type, Element_Type, "=");
      with procedure Read_Element
        (Self : in out Checkpoint_Load_State; Element : out Element_Type);
   procedure Read_Vector
     (Self : in out Checkpoint_Load_State; Vector : out Vectors.Vector);
   --  Generic implementation to read a vector tree from a checkpoint

   procedure Read (Self : in out Checkpoint_Load_State; Value : out U8_Array);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out CU_Id);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Pc_Type);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out SCO_Id);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Source_File_Index);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Source_Location);
   procedure Read (Self : in out Checkpoint_Load_State; Value : out String);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Tristate);
   procedure Read
     (Self : in out Checkpoint_Load_State; Value : out Unbounded_String);

   ---------------------------------------------
   -- Helpers to implement checkpoints saving --
   ---------------------------------------------

   --  All the Write* procedures below serialize specific data types to
   --  checkpoints. There is no overload (functions have different names) for
   --  numeric data types in order to avoid ambiguities when writing for
   --  instance:
   --
   --     CSS.Write_U8 (My_Type'Pos (Numeric_Data));
   --
   --  Also define generic procedures to write containers (maps, multiway
   --  trees, sets, vectors).

   procedure Write_BDD_Node
     (Self : in out Checkpoint_Save_State; Value : BDD_Node_Id);
   procedure Write_Bit_Id
     (Self : in out Checkpoint_Save_State; Value : Any_Bit_Id);
   procedure Write_CU (Self : in out Checkpoint_Save_State; Value : CU_Id);
   procedure Write_Condition
     (Self : in out Checkpoint_Save_State; Value : Any_Condition_Index);
   procedure Write_Count
     (Self : in out Checkpoint_Save_State; Value : Count_Type);
   procedure Write_I32
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Integer_32);
   procedure Write_Inst (Self : in out Checkpoint_Save_State; Value : Inst_Id);
   procedure Write_Integer
     (Self : in out Checkpoint_Save_State; Value : Integer);
   procedure Write_PC (Self : in out Checkpoint_Save_State; Value : Pc_Type);
   procedure Write_SCO (Self : in out Checkpoint_Save_State; Value : SCO_Id);
   procedure Write_SFI
     (Self : in out Checkpoint_Save_State; Value : Source_File_Index);
   procedure Write_U8
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_8);
   procedure Write_U16
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_16);
   procedure Write_U32
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_32);
   procedure Write_U64
     (Self : in out Checkpoint_Save_State; Value : Interfaces.Unsigned_64);

   procedure Write (Self : in out Checkpoint_Save_State; Value : Boolean);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Compilation_Unit);
   procedure Write
     (Self  : in out Checkpoint_Save_State;
      Value : SC_Obligations.Fingerprint_Type);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Supported_Language_Kind);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Any_Line_State);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Local_Source_Location);
   procedure Write
     (Self  : in out Checkpoint_Save_State;
      Value : Local_Source_Location_Range);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Source_Location);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Source_Location_Range);
   procedure Write (Self : in out Checkpoint_Save_State; Value : String);
   procedure Write (Self : in out Checkpoint_Save_State; Value : Tristate);
   procedure Write
     (Self : in out Checkpoint_Save_State; Value : Unbounded_String);

   procedure Write_Unbounded
     (Self : in out Checkpoint_Save_State; Value : String);

   generic
      type Key_Type is private;
      type Element_Type is private;
      type Map_Type is private;
      type Cursor_Type is private;

      with function Length (Self : Map_Type) return Count_Type is <>;
      with procedure Iterate
        (Self    : Map_Type;
         Process : not null access procedure (Position : Cursor_Type));
      with procedure Query_Element
        (Position : Cursor_Type;
         Process  : not null access
                      procedure (Key : Key_Type; Element : Element_Type));

      with procedure Write_Key
        (Self : in out Checkpoint_Save_State; Key : Key_Type);
      with procedure Write_Element
        (Self : in out Checkpoint_Save_State; Element : Element_Type);
   procedure Write_Map
     (Self : in out Checkpoint_Save_State; Map : Map_Type);
   --  Generic implementation to write a map to a checkpoint. Since there are
   --  two flavors for maps (ordered and hashed), do not take a instantiated
   --  formal package, but rather the only three map primitives that we need:
   --  Length, Iterate and Query_Element.

   generic
      type Element_Type is private;
      type Set_Type is private;
      type Cursor_Type is private;

      with function Length (Self : Set_Type) return Count_Type is <>;
      with procedure Iterate
        (Self    : Set_Type;
         Process : not null access procedure (Position : Cursor_Type));
      with procedure Query_Element
        (Position : Cursor_Type;
         Process  : not null access procedure (Element : Element_Type));

      with procedure Write_Element
        (Self : in out Checkpoint_Save_State; Element : Element_Type);
   procedure Write_Set
     (Self : in out Checkpoint_Save_State; Set : Set_Type);
   --  Generic implementation to write a set to a checkpoint. Since there are
   --  two flavors for sets (ordered and hashed), do not take a instantiated
   --  formal package, but rather the only three set primitives that we need:
   --  Length, Iterate and Query_Element.

   generic
      type Element_Type is private;
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
      with package Multiway_Trees is new Ada.Containers.Multiway_Trees
        (Element_Type, "=");
      with procedure Write_Element
        (Self : in out Checkpoint_Save_State; Element : Element_Type);
   procedure Write_Tree
     (Self : in out Checkpoint_Save_State; Tree : Multiway_Trees.Tree);
   --  Generic implementation to write a multiway tree from a checkpoint

   generic
      type Index_Type is range <>;
      type Element_Type is private;
      with function "=" (Left, Right : Element_Type) return Boolean is <>;
      with package Vectors is new Ada.Containers.Vectors
        (Index_Type, Element_Type, "=");
      with procedure Write_Element
        (Self : in out Checkpoint_Save_State; Element : Element_Type);
   procedure Write_Vector
     (Self : in out Checkpoint_Save_State; Vector : Vectors.Vector);
   --  Generic implementation to write a vector tree to a checkpoint

private

   type SFI_Map_Array is
     array (Source_File_Index range <>) of Source_File_Index;
   type SFI_Map_Acc is access all SFI_Map_Array;

   type CU_Id_Map_Array is array (CU_Id range <>) of CU_Id;
   type CU_Id_Map_Acc is access all CU_Id_Map_Array;

   type Inst_Id_Map_Array is array (Inst_Id range <>) of Inst_Id;
   type Inst_Id_Map_Acc is access all Inst_Id_Map_Array;

   type BDD_Node_Id_Map_Array is array (BDD_Node_Id range <>) of BDD_Node_Id;
   type BDD_Node_Id_Map_Acc is access all BDD_Node_Id_Map_Array;

   type SCO_Id_Map_Array is array (SCO_Id range <>) of SCO_Id;
   type SCO_Id_Map_Acc is access all SCO_Id_Map_Array;

   type SCO_Ignored_Map_Array is array (SCO_Id range <>) of Boolean;
   type SCO_Ignored_Map_Access is access all SCO_Ignored_Map_Array;

   type SFI_Ignored_Map_Array is array (Source_File_Index range <>) of Boolean;
   type SFI_Ignored_Map_Access is access all SFI_Ignored_Map_Array;

   type CU_Id_Ignored_Map_Array is array (CU_Id range <>) of Boolean;
   type CU_Id_Ignored_Access is access all CU_Id_Ignored_Map_Array;

   type SFI_Simple_Name_Map_Array is
     array (Source_File_Index range <>) of Unbounded_String;
   type SFI_Simple_Name_Map_Access is access all SFI_Simple_Name_Map_Array;

   type Checkpoint_Relocations is  record
      SFI_Map  : SFI_Map_Acc;
      CU_Map   : CU_Id_Map_Acc;
      Inst_Map : Inst_Id_Map_Acc;
      BDD_Map  : BDD_Node_Id_Map_Acc;
      SCO_Map  : SCO_Id_Map_Acc;
      --  Maps to replace checkpoint identifiers with local table identifiers
      --  after merging the checkpoint in local tables.

      SFI_Simple_Filenames : SFI_Simple_Name_Map_Access;
      --  For each SFI from this checkpoint, corresponding simple name

      Ignored_SCOs : SCO_Ignored_Map_Access;
      --  Map of SCOs that were ignored and thus are not remapped when loading
      --  the checkpoint.

      Ignored_SFIs : SFI_Ignored_Map_Access;
      Ignored_CUs  : CU_Id_Ignored_Access;
      --  Represents the ignored Source files and compilation units that
      --  we should not load from the SID file.
      --  Unused when loading data from a consolidation checkpoint.

   end record;

end Checkpoints;
