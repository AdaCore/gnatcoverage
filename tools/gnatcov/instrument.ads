------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

--  Support for source instrumentation

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with GPR2.Build.Source;
with GPR2.Project.View;

with Types; use Types;

with Checkpoints;    use Checkpoints;
with Files_Handling;
with Logging;
with SC_Obligations; use SC_Obligations;
with Strings;        use Strings;
with Subprocesses;   use Subprocesses;
with Switches;       use Switches;
with Traces_Source;  use Traces_Source;

package Instrument is

   Clean_Objdirs_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("INSTRUMENT_CLEAN_OBJDIRS");
   --  Trace to show details about how object directories in GPR projects are
   --  cleaned.

   Sources_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("INSTRUMENT_SOURCES");
   --  Trace to show details about written instrumented sources

   use type Ada.Containers.Count_Type;
   use all type Unbounded_String;

   Parallelism_Level : Natural := 1;

   type Instrumentation_Mode is
     (Project_Instrumentation, Integrated_Instrumentation);

   function Language_Kind
     (Language : Some_Language) return Supported_Language_Kind;
   --  Returns the language kind (unit-based or file-based) for the given
   --  language.

   type Ada_Identifier is new Unbounded_String;
   --  Simple Ada identifier

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out Ada_Identifier);
   --  Read an Ada_Identifier from CLS

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : Ada_Identifier);
   --  Write an Ada_Identifier to CSS

   package Ada_Identifier_Vectors is new Ada.Containers.Vectors
     (Positive, Ada_Identifier);

   subtype Ada_Qualified_Name is Ada_Identifier_Vectors.Vector;
   --  Sequence of ada identifiers, representing a qualified name. For
   --  instance: Scope_A.Scope_B.Scope_C

   function "&" (Left, Right : Ada_Qualified_Name) return Ada_Qualified_Name
      renames Ada_Identifier_Vectors."&";

   function To_Ada (Name : Ada_Qualified_Name) return String;
   --  Turn the given qualified name into Ada syntax

   procedure Read is new Read_Vector
     (Index_Type   => Positive,
      Element_Type => Ada_Identifier,
      Vectors      => Ada_Identifier_Vectors,
      Read_Element => Read);

   procedure Write is new Write_Vector
     (Index_Type    => Positive,
      Element_Type  => Ada_Identifier,
      Vectors       => Ada_Identifier_Vectors,
      Write_Element => Write);

   type Compilation_Unit_Part
     (Language_Kind : Supported_Language_Kind := Unit_Based_Language)
   is record

      case Language_Kind is
         when Unit_Based_Language =>
            Unit : Ada_Qualified_Name   := Ada_Identifier_Vectors.Empty_Vector;
            Part : GPR2.Valid_Unit_Kind := GPR2.S_Body;
            --  Identifies an Ada compilation unit (unit-based)

         when File_Based_Language =>
            Filename : Unbounded_String;
            --  Fallback for file-based languages (like C). We use the full
            --  filename, for homonym resiliency.

      end case;
   end record;
   --  Unique identifier for an instrumented unit part

   procedure Read
     (CLS   : in out Checkpoints.Checkpoint_Load_State;
      Value : out Compilation_Unit_Part);
   --  Read a Compilation_Unit_Part from CLS

   procedure Write
     (CSS   : in out Checkpoints.Checkpoint_Save_State;
      Value : Compilation_Unit_Part);
   --  Write a Compilation_Unit_Part to CSS

   Part_Tags : constant array (GPR2.Valid_Unit_Kind) of Character :=
     (GPR2.S_Spec     => 'S',
      GPR2.S_Body     => 'B',
      GPR2.S_Separate => 'U');

   function "=" (Left, Right : Compilation_Unit_Part) return Boolean;

   function "<" (Left, Right : Compilation_Unit_Part) return Boolean;
   --  Compare the result of a call to Instrumented_Unit_Slug (which gives
   --  unique identifiers for each compilation unit name) for both operands.

   function Image (CU_Name : Compilation_Unit_Part) return String;
   --  Return a string representation of CU_Name for use in diagnostics

   function Qualified_Name_Slug
     (Name     : Ada_Qualified_Name;
      Use_Hash : Boolean := not Switches.Use_Full_Slugs) return String;
   --  Given a qualified name, return a unique identifier to describe it.
   --  This identifier is an 32bit hash of the identifiers in Name, if Use_Hash
   --  is True, otherwise, it remains human readable.
   --
   --  This identifier can be used as a filename suffix / unit name, as it does
   --  not contain any '-'.

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Part) return String;
   --  Given a unit to instrument, return a unique identifier to describe it
   --  (the so called slug).
   --
   --  One can use this slug to generate unique names for this unit.

   function Filename_Slug
     (Fullname : String;
      Use_Hash : Boolean := not Switches.Use_Full_Slugs) return String;
   --  Given a filename to instrument, return a unique identifier to describe
   --  it (the so called slug). This is a hash of the filename if Use_Hash is
   --  True, otherwise a human-readable slug of the base name with the same
   --  hash concatenated at the end, to distinguish slugs from homonym files.
   --
   --  One can use this slug to generate unique names for this unit.

   function To_Qualified_Name (Name : String) return Ada_Qualified_Name;
   --  Convert a String qualified name into our format

   function Canonicalize (Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Fold casing of Ada identifiers

   function To_Symbol_Name (Name : Ada_Qualified_Name) return String;
   --  Lower case each name of the qualified name, and joined them with an
   --  underscore, to have a C-like syntax.
   --
   --  Example: passing the qualified name Foo.Bar will return the string
   --  "foo_bar".

   function Has_Prefix (Name, Prefix : Ada_Qualified_Name) return Boolean is
     (Prefix.Last_Index <= Name.Last_Index
      and then
        (for all I in 1 .. Prefix.Last_Index
         => Prefix.Constant_Reference (I) = Name.Constant_Reference (I)));
   --  Returns whether Name starts with the same identifiers as Prefix, case
   --  sensitive.

   function CU_Name_For_Unit
     (Unit : Ada_Qualified_Name;
      Part : GPR2.Valid_Unit_Kind) return Compilation_Unit_Part;
   --  Return the compilation unit name for the Ada compilation unit
   --  corresponding to the unit name and the unit part parameters.

   function CU_Name_For_File
     (Filename : Unbounded_String) return Compilation_Unit_Part;
   --  Return the compilation unit name for the C translation unit
   --  corresponding to the filename parameter.

   function To_Compilation_Unit_Name
     (Source : GPR2.Build.Source.Object) return Compilation_Unit_Part;
   --  Return the compilation unit name corresponding to the unit in Source

   package Instrumented_Unit_To_CU_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Part,
      Element_Type => CU_Id);

   procedure Read is new Read_Map
     (Key_Type     => Compilation_Unit_Part,
      Element_Type => CU_Id,
      Map_Type     => Instrumented_Unit_To_CU_Maps.Map,
      Clear        => Instrumented_Unit_To_CU_Maps.Clear,
      Insert       => Instrumented_Unit_To_CU_Maps.Insert,
      Read_Key     => Read,
      Read_Element => Read);

   procedure Write is new Write_Map
     (Key_Type      => Compilation_Unit_Part,
      Element_Type  => CU_Id,
      Map_Type      => Instrumented_Unit_To_CU_Maps.Map,
      Cursor_Type   => Instrumented_Unit_To_CU_Maps.Cursor,
      Length        => Instrumented_Unit_To_CU_Maps.Length,
      Iterate       => Instrumented_Unit_To_CU_Maps.Iterate,
      Query_Element => Instrumented_Unit_To_CU_Maps.Query_Element,
      Write_Key     => Write,
      Write_Element => Write_CU);

   Instrumented_Unit_CUs : Instrumented_Unit_To_CU_Maps.Map;
   --  Associate a CU id for all instrumented units. Updated each time we
   --  instrument a unit (or load a checkpoint) and used each time we read a
   --  coverage buffer (or save to a checkpoint).

   package SFI_To_PP_Cmd_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Source_File_Index,
        Element_Type => Command_Type);

   procedure Read is new Read_Map
     (Key_Type     => Source_File_Index,
      Element_Type => Command_Type,
      Map_Type     => SFI_To_PP_Cmd_Maps.Map,
      Clear        => SFI_To_PP_Cmd_Maps.Clear,
      Insert       => SFI_To_PP_Cmd_Maps.Insert,
      Read_Key     => Read,
      Read_Element => Read);

   procedure Write is new Write_Map
     (Key_Type      => Source_File_Index,
      Element_Type  => Command_Type,
      Map_Type      => SFI_To_PP_Cmd_Maps.Map,
      Cursor_Type   => SFI_To_PP_Cmd_Maps.Cursor,
      Length        => SFI_To_PP_Cmd_Maps.Length,
      Iterate       => SFI_To_PP_Cmd_Maps.Iterate,
      Query_Element => SFI_To_PP_Cmd_Maps.Query_Element,
      Write_Key     => Write_SFI,
      Write_Element => Write);

   PP_Cmds : SFI_To_PP_Cmd_Maps.Map;
   --  Save the preprocessing command for each unit that supports it

   function Find_Instrumented_Unit
     (CU_Name : Compilation_Unit_Part) return CU_Id;
   --  Return the CU_Id corresponding to the given instrumented unit, or
   --  No_CU_Id if not found.

   type Lang_Array is array (Src_Supported_Language range <>)
     of Unbounded_String;
   type C_Lang_Array_Vec is array (C_Family_Language) of String_Vectors.Vector;

   type Naming_Scheme_Desc is record
      Spec_Suffix, Body_Suffix : Lang_Array (Src_Supported_Language);
      --  Suffixes for the body and the spec

      Dot_Replacement : Unbounded_String;
      --  Character to use as identifier separator for file naming (used for
      --  unit-based languages).
   end record;

   type Prj_Desc is record
      Prj_Name : Ada_Qualified_Name;
      --  Name for the project

      Output_Dir : Unbounded_String;
      --  Where the instrumented sources and coverage buffer units are
      --  generated.

      Naming_Scheme : Naming_Scheme_Desc;
      --  Naming scheme for this project

      Compiler_Driver : Lang_Array (C_Family_Language);
      --  Compiler used to compile the sources

      Compiler_Options : C_Lang_Array_Vec;
      --  For languages resorting to the compiler to preprocess sources, list
      --  of compiler switches to pass to the preprocessor invocation.

      Compiler_Options_Unit : Files_Handling.File_To_String_Vectors_Maps.Map;
      --  Compiler switches applying to a specific unit

      Search_Paths : String_Vectors.Vector;
      --  List of compiler switches to look up the project source directories

   end record;
   --  This record stores the information that is required from the project
   --  for instrumentation purposes.

   type Prj_Desc_Access is access Prj_Desc;

   function Load_From_Command_Line return Prj_Desc;

   function Unparse
     (Desc      : Prj_Desc;
      Unit_Name : Unbounded_String;
      Lang      : Src_Supported_Language) return String_Vectors.Vector;
   --  Return a list of command line switches holding all the project
   --  information for the given Unit_Name of the language Lang.

   function Instrumentation_Tag return String;
   --  Generate a unique identifier that can be used to tag the current
   --  instrumentation run.

end Instrument;
