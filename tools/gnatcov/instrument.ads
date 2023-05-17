------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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
with Ada.Strings.Unbounded;

with GNAT.Regexp;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Types; use Types;

with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with SC_Obligations;      use SC_Obligations;
with Subprocesses;        use Subprocesses;
with Strings;             use Strings;
with Switches;            use Switches;

package Instrument is

   package US renames Ada.Strings.Unbounded;

   function Language_Kind
     (Language : Some_Language) return Any_Language_Kind;
   --  Returns the language kind (unit-based or file-based) for the given
   --  language.

   type Ada_Identifier is new Ada.Strings.Unbounded.Unbounded_String;
   --  Simple Ada identifier

   package Ada_Identifier_Vectors is new Ada.Containers.Vectors
     (Positive, Ada_Identifier);

   subtype Ada_Qualified_Name is Ada_Identifier_Vectors.Vector;
   --  Sequence of ada identifiers, representing a qualified name. For
   --  instance: Scope_A.Scope_B.Scope_C

   function "&" (Left, Right : Ada_Qualified_Name) return Ada_Qualified_Name
      renames Ada_Identifier_Vectors."&";

   function To_Ada (Name : Ada_Qualified_Name) return String
     with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into Ada syntax

   type Compilation_Unit_Name
     (Language_Kind : Any_Language_Kind := Unit_Based_Language)
   is record

      case Language_Kind is
         when Unit_Based_Language =>
            Unit : Ada_Qualified_Name := Ada_Identifier_Vectors.Empty_Vector;
            Part : Unit_Parts         := Unit_Body;
            --  Identifies an Ada compilation unit (unit-based)

         when File_Based_Language =>
            Filename : US.Unbounded_String;
            --  Fallback for file-based languages (like C). We will use the
            --  simple filename for now.

      end case;
   end record;
   --  Unique identifier for an instrumented unit

   Part_Tags : constant array (Unit_Parts) of Character :=
     (Unit_Spec     => 'S',
      Unit_Body     => 'B',
      Unit_Separate => 'U');

   function "=" (Left, Right : Compilation_Unit_Name) return Boolean;

   function "<" (Left, Right : Compilation_Unit_Name) return Boolean;
   --  Compare the result of a call to Instrumented_Unit_Slug (which gives
   --  unique identifiers for each compilation unit name) for both operands.

   function Image (CU_Name : Compilation_Unit_Name) return String;
   --  Return a string representation of CU_Name for use in diagnostics

   function Qualified_Name_Slug (Name : Ada_Qualified_Name) return String;
   --  Given a qualified name, return a unique identifier to describe it. This
   --  identifier can be used as a filename suffix / unit name, as it does
   --  not contain any '-'.

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Name) return String;
   --  Given a unit to instrument, return a unique identifier to describe it
   --  (the so called slug).
   --
   --  One can use this slug to generate unique names for this unit.

   function Filename_Slug (Fullname : String) return String;
   --  Given a filename to instrument, return a unique identifier to describe
   --  it (the so called slug).
   --
   --  One can use this slug to generate unique names for this unit.

   function To_Qualified_Name (Name : String) return Ada_Qualified_Name;
   --  Convert a String qualified name into our format

   function Canonicalize (Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Fold casing of Ada identifiers

   function To_Symbol_Name (Name : Ada_Qualified_Name) return String
      with Pre => not Name.Is_Empty;
   --  Lower case each name of the qualified name, and joined them with an
   --  underscore, to have a C-like syntax.
   --
   --  Example: passing the qualified name Foo.Bar will return the string
   --  "foo_bar".

   function CU_Name_For_Unit
     (Unit : Ada_Qualified_Name;
      Part : Unit_Parts) return Compilation_Unit_Name;
   --  Return the compilation unit name for the Ada compilation unit
   --  corresponding to the unit name and the unit part parameters.

   function CU_Name_For_File
     (Filename : US.Unbounded_String) return Compilation_Unit_Name;
   --  Return the compilation unit name for the C translation unit
   --  corresponding to the filename parameter.

   function To_Compilation_Unit_Name
     (Source_File : GNATCOLL.Projects.File_Info) return Compilation_Unit_Name;
   --  Return the compilation unit name corresponding to the unit in
   --  Source_File.

   function To_Filename
     (Project  : Project_Type;
      CU_Name  : Compilation_Unit_Name;
      Language : Any_Language) return String;
   --  Return the name of the file to contain the given compilation unit,
   --  according to Project's naming scheme.

   package Instrumented_Unit_To_CU_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Compilation_Unit_Name,
      Element_Type => CU_Id);

   Instrumented_Unit_CUs : Instrumented_Unit_To_CU_Maps.Map;
   --  Associate a CU id for all instrumented units. Updated each time we
   --  instrument a unit (or load a checkpoint) and used each time we read a
   --  coverage buffer (or save to a checkpoint).

   package SFI_To_PP_Cmd_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Source_File_Index,
        Element_Type => Command_Type);

   PP_Cmds : SFI_To_PP_Cmd_Maps.Map;
   --  Save the preprocessing command for each unit that supports it

   function Find_Instrumented_Unit
     (CU_Name : Compilation_Unit_Name) return CU_Id;
   --  Return the CU_Id corresponding to the given instrumented unit, or
   --  No_CU_Id if not found.

   procedure Instrument_Units_Of_Interest
     (Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp;
      Mains                : String_Vectors.Vector);
   --  Generate instrumented sources for the source files of all units of
   --  interest. Also save mappings between coverage buffers and SCOs for each
   --  library units to SID files (one per library unit).
   --
   --  Depending on Dump_Config, instrument mains to schedule a call to the
   --  dump procedure for the list of coverage buffers in all mains in the
   --  project.
   --
   --  Language_Version restricts what source constructs the instrumenter is
   --  allowed to use. For instance, if Ada_2005 (or a lower version) is
   --  passed, it will not be allowed to introduce expression functions, and
   --  thus will emit a warning when it needed to do so.
   --
   --  If Ignored_Source_File is non-null, ignore files whose names match the
   --  accessed pattern.
   --
   --  Mains is the list of source files that were listed on the command line:
   --  if non-empty, they replace the mains specified in project files.
end Instrument;
