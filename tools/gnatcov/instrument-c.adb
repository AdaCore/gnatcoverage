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

with Ada.Characters.Handling;
with Ada.Containers;  use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with Clang.CX_String;  use Clang.CX_String;
with Clang.Extensions; use Clang.Extensions;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.VFS;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Command_Line;
with ALI_Files;           use ALI_Files;
with Coverage;            use Coverage;
with Coverage_Options;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with Hex_Images;          use Hex_Images;
with Instrument.C_Utils;  use Instrument.C_Utils;
with Outputs;             use Outputs;
with Paths;               use Paths;
with SCOs;
with Subprocesses;        use Subprocesses;
with System;              use System;
with Table;
with Text_Files;          use Text_Files;

package body Instrument.C is

   package GPR renames GNATCOLL.Projects;
   package US renames Ada.Strings.Unbounded;

   function To_Chars_Ptr_Array
     (V : String_Vectors.Vector) return chars_ptr_array;
   --  Convert a string vector to a chars_ptr_array. Result must be freed by
   --  the caller.

   procedure Free (Self : in out chars_ptr_array);
   --  Free all strings in Self

   function Compiler_Driver
     (Project  : GNATCOLL.Projects.Project_Type;
      Language : C_Family_Language) return String;
   --  Return the command name for the compiler for Language in the given
   --  Project.

   function Source_Suffix
     (Instrumenter : C_Family_Instrumenter_Type'Class;
      Part         : GNATCOLL.Projects.Unit_Parts;
      Project      : GNATCOLL.Projects.Project_Type) return String;
   --  Return the filename suffix corresponding for Part files and the language
   --  that Instrumenter handles in the given project.

   procedure Filter_Annotations;
   --  Remove any exemption annotations from the map that intersects a
   --  statement SCO. This is not part of Import_Annotations as this is only
   --  required for C annotations, where the exemption markers can be
   --  arbitrarily placed. This would also not work for Ada exemptions as there
   --  is always a SCO associated with a pragma exempt, which would result in
   --  all the annotations being filtered out.

   function Last_File return Valid_Source_File_Index
   is (SCOs.SCO_Unit_Table.Table (SCOs.SCO_Unit_Table.Last).File_Index);
   --  Return the source file for the last low-level SCO that was created

   ------------------------------
   --  Preprocessing utilities --
   ------------------------------

   Macro_Def_Regexp : constant Pattern_Matcher := Compile (
     "#define"
     & "(?: |\t)+"
     --  "#define", then a non-empty blank

     & "([a-zA-Z0-9_][^ \t]*"
     --  The name of the macro, followed by....

     & "(?:\(.*\))?"
     --  The optional list of macro arguments

     & ")"
     --  End of "extended" macro name (actual name + arguments)

     & "(.*)"
     --  The macro value itself
   );
   --  Regular expression to analyze definitions for builtin macros (see
   --  Builtin_Macros)

   type Macro_Vector_Access is access Macro_Vectors.Vector;
   type Macro_Vector_Cst_Access is access constant Macro_Vectors.Vector;
   package Compiler_Macros_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Macro_Vector_Access,
      Equivalent_Keys => "=",
      Hash            => Ada.Strings.Unbounded.Hash);
   Compiler_Macros : Compiler_Macros_Maps.Map;
   --  Cache for computed compiler builtin macros, to avoid relaunching the
   --  compiler command every time a file is instrumented. Used in the
   --  Builtin_Macros function only.

   --  The three following functions are not used, but could be in the future,
   --  when we will refine what is done with macros.

   function Builtin_Macros
     (Lang, Compiler, Std, Output_Dir : String) return Macro_Vector_Cst_Access;
   --  Return the list of built-in macros for the given compiler, standard and
   --  language. Output_Dir is used to store a temporary file.
   --
   --  Note that we could generate a fully-fledged preprocessor configuration
   --  (the standard macros + the command-line defined macros with an
   --  additional argument there), but it is more convenient to cache the
   --  "light" preprocessor configuration that is determined by the compiler,
   --  language and standard only.

   procedure Preprocess_Source
     (Filename     : String;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      PP_Filename  : out Unbounded_String;
      Info         : in out Project_Info;
      Options      : in out Analysis_Options);
   --  Preprocess the source at Filename and extend Options using the
   --  preprocessor output.
   --
   --  This uses the compiler in the Compiler_Driver project attribute to
   --  preprocess the file, assuming that it accepts the -E flag, to preprocess
   --  a file.

   function Common_Parse_TU_Args return String_Vectors.Vector;
   --  Return the list of arguments that should always be passed to
   --  Parse_Translation_Unit.

   ---------------------------
   --  Passes specificities --
   ---------------------------

   type Record_PP_Info_Pass_Kind is new Pass_Kind with null record;

   overriding procedure Append_SCO
     (Pass               : Record_PP_Info_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Local_Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a SCO to SCOs.SCO_Table. Also partially fill the preprocessing
   --  info: the actual source range referred, and the expanded macro name, if
   --  this is a SCO inside a macro expansion.

   type Instrument_Pass_Kind is new Pass_Kind with null record;

   overriding procedure Append_SCO
     (Pass               : Instrument_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Local_Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a SCO to SCOs.SCO_Table, and complete the preprocessing info with
   --  the preprocessed source range.

   overriding procedure Instrument_Statement
     (Pass         : Instrument_Pass_Kind;
      UIC          : in out C_Unit_Inst_Context'Class;
      LL_SCO       : Nat;
      Insertion_N  : Cursor_T;
      Instr_Scheme : Instr_Scheme_Type);
   --  Add an entry to UIC.Source_Statements

   overriding procedure Instrument_Decision
     (Pass     : Instrument_Pass_Kind;
      UIC      : in out C_Unit_Inst_Context'Class;
      LL_SCO   : Nat;
      Decision : Cursor_T;
      State    : US.Unbounded_String);
   --  Add an entry to UIC.Source_Decisions

   overriding procedure Instrument_Condition
     (Pass      : Instrument_Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : US.Unbounded_String;
      First     : Boolean);
   --  Add an entry to UIC.Source_Conditions

   overriding procedure Curlify
     (Pass : Instrument_Pass_Kind;
      N    : Cursor_T;
      Rew  : Rewriter_T);
   --  Wrapper around Instrument.C.Utils.Curlify

   overriding procedure Insert_MCDC_State
     (Pass       : Instrument_Pass_Kind;
      UIC        : in out C_Unit_Inst_Context'Class;
      Name       : String;
      MCDC_State : out US.Unbounded_String);
   --  Wrapper around Insert_MCDC_State overload

   Record_PP_Info_Pass : aliased Record_PP_Info_Pass_Kind;
   Instrument_Pass     : aliased Instrument_Pass_Kind;

   -------------------------------------
   -- Generation of witness fragments --
   -------------------------------------

   function Buffers_Subscript (Buffers_Index : Positive) return String
   is ("[" & Img (Buffers_Index - 1) & "]");
   --  Return a C array subscript to refer to the coverage buffers
   --  corresponding to Buffers_Index.
   --
   --  Note that while Buffers_Index is 1-based, C arrays are 0-based, hence
   --  the index "off-by-1" conversion.

   function Make_Expr_Witness
     (UIC          : C_Unit_Inst_Context;
      Buffer_Index : Positive;
      Bit          : Bit_Id) return String;
   --  Create a procedure call expression on to witness execution of the low
   --  level SCO with the given Bit id in the statement buffer at Buffer_Index.

   function Make_Statement_Witness
     (UIC          : C_Unit_Inst_Context;
      Buffer_Index : Positive;
      Bit          : Bit_Id) return String;
   --  Create a procedure call statement to witness execution of the low level
   --  SCO with the given Bit id in the statement buffer at Buffer_Index.

   procedure Insert_Statement_Witness
     (UIC           : in out C_Unit_Inst_Context;
      Buffers_Index : Positive;
      SS            : C_Source_Statement);
   --  Insert witness function call for the identified statement. Buffers_Index
   --  designates the relevant set of coverage buffers.

   procedure Insert_Decision_Witness
     (UIC           : in out C_Unit_Inst_Context;
      Buffers_Index : Positive;
      SD            : C_Source_Decision;
      Path_Count    : Natural);
   --  For use when decision coverage or MC/DC is requested. Insert witness
   --  function call for the identified decision. Buffers_Index designates the
   --  relevant set of coverage buffers.

   procedure Insert_Condition_Witness
     (UIC    : in out C_Unit_Inst_Context;
      SC     : C_Source_Condition;
      Offset : Natural);
   --  For use when MC/DC coverage requested. Insert witness function call for
   --  the identified condition.

   function Make_MCDC_State_Name (LL_SCO_Id : Nat) return String is
     ("mcdc_state_" & Img (Integer (LL_SCO_Id)));
   --  Return the name of the MC/DC state local variable for the given
   --  decision SCO.

   function Insert_MCDC_State
     (UIC : in out C_Unit_Inst_Context'Class; Name : String) return String;

   ----------------------------
   -- Source level rewriting --
   ----------------------------

   procedure Record_PP_Info
     (Unit_Info : Instrumented_Unit_Info;
      IC        : in out Inst_Context;
      UIC       : in out C_Unit_Inst_Context);
   --  Emit the low-level SCOs for the given unit. Do not process them: this is
   --  left to the other (instrumentation) pass.

   procedure Instrument_Source_File
     (CU_Name      : Compilation_Unit_Name;
      Unit_Info    : Instrumented_Unit_Info;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj_Info     : in out Project_Info;
      IC           : in out Inst_Context;
      UIC          : out C_Unit_Inst_Context);
   --  Generate the instrumented source corresponding to CU_Name/Unit_Info.
   --  Record instrumentation information in IC.
   --
   --  If the unit to instrument is also a main and the buffers dump trigger
   --  is not manual, instrumented code will also dump the coverage buffers.

   ----------------------------
   -- Source instrumentation --
   ----------------------------

   function Unit_Buffers_Array_Name (IC : Inst_Context) return String is
      ("gnatcov_rts_buffers_array_" & (+IC.Project_Name));
   --  Name of the symbol that references the
   --  gnatcov_rts_coverage_buffers_array struct (defined for the whole
   --  project). This struct is an array containing the coverage buffers of all
   --  of the instrumented units.
   --
   --  We need this to be unique per root project instrumented, as gnatcov
   --  gives the possibility to link two separately-instrumented libraries in
   --  the same executable.

   function Buffers_List_Filename (IC : Inst_Context) return String is
     ("gnatcov_rts_c-buffers-lists-" & (+IC.Project_Name));
   --  Return the name of the unit containing the array of coverage buffers

   procedure Emit_Buffer_Unit
     (Info         : in out Project_Info;
      UIC          : C_Unit_Inst_Context'Class;
      Instrumenter : C_Family_Instrumenter_Type'Class);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit, for the given instrumenter.

   procedure Emit_Dump_Helper_Unit
     (IC           : Inst_Context;
      Info         : in out Project_Info;
      Main         : Compilation_Unit_Name;
      Helper_Unit  : out US.Unbounded_String;
      Instrumenter : C_Family_Instrumenter_Type'Class);
   --  Emit the unit to contain helpers to implement the automatic dump of
   --  coverage buffers for the given Main unit. Info must be the project that
   --  owns this main. Upon return, the name of this helper unit is stored in
   --  Helper_Unit.

   procedure Apply (Self : in out C_Source_Rewriter);
   --  Overwrite the file with the rewritter modifications

   procedure Start_Rewriting
     (Self         : out C_Source_Rewriter;
      Info         : in out Project_Info;
      Filename     : String;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Preprocessed : Boolean := False);
   --  Start a rewriting session for the given file identified by its full
   --  name.
   --
   --  If Preprocessed is set to True, consider that the file was preprocessed
   --  beforehand. Otherwise, generate a preprocessed version of it in
   --  Info.Output_Dir and start a rewriting session on the latter.

   procedure Run_Diagnostics (TU : Translation_Unit_T);
   --  Output clang diagnostics on the given translation unit

   procedure Auto_Dump_Buffers_In_Main
     (IC           : Inst_Context;
      Info         : in out Project_Info;
      Main         : Compilation_Unit_Name;
      Rew          : in out C_Source_Rewriter;
      Instrumenter : C_Family_Instrumenter_Type'Class);
   --  Common code for auto dump insertion in the "main" function, used in the
   --  Auto_Dump_Buffers_In_Main primitive for C_Instrumenter_Type, and from
   --  the Instrument_Source_File procedure.
   --
   --  Arguments have the same semantics as in the Auto_Dump_Buffers_In_Main
   --  primitive. The additional Rew argument is the C source rewriter that is
   --  ready to use for the source file to instrument.

   function Format_Str_Constant (Value : String) return String;
   --  Return a gnatcov_rts_string literal corresponding to Value

   function Format_Array_Init_Expr
     (Exprs     : String_Vectors.Vector;
      Multiline : Boolean := False) return String;
   --  Helper to format the initialization expression for an array.
   --
   --  Exprs is the list of expressions for all items in the array.
   --
   --  If Multiline is False, the returned expression fits on a single line.
   --  Otherwise, put one array item per line.

   function Format_Fingerprint
     (Fingerprint : SC_Obligations.SCOs_Hash) return String;
   --  Helper to format a uint8_t[] literal for a SCOs fingerprint

   function Format_Def
     (Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Func_Args    : String := "";
      Init_Expr    : String := "";
      External     : Boolean := False) return String;
   --  Helper to format a variable/constant definition.
   --
   --  If Instrumenter is for C++, return first an "extern ""C""" declaration
   --  for the definition to set the C linkage, then the definition itself.
   --
   --  C_Type is the type for the declared entity ("int", "const char *", ...).
   --
   --  Name is the identifier for the declared entity.
   --
   --  If the declared entity is an array, Array_Size must be the
   --  representation of its size. Note that in this case, C_Type is the type
   --  of the elements in this array. Leave empty otherwise.
   --
   --  If the declared entity is a function, Func_Args must be the list of its
   --  arguments. Note that in that case, C_Type is the function return type.
   --  Leave empty otherwise.
   --
   --  Init_Expr is the initialization expression for the declared entity, if
   --  needed.
   --
   --  If External is True, add an "extern" keyword as a prefix for the
   --  declaration. Note that this is incompatible with Init_Expr, as an extern
   --  declaration be a definition.

   procedure Put_Format_Def
     (File         : in out Text_Files.File_Type;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Init_Expr    : String := "");
   --  Like Format_Def, but write the definition to File

   function Format_Extern_Decl
     (Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Func_Args    : String := "") return String;
   --  Helper for format an "extern" declaration. Arguments are the same as
   --  Format_Def.

   procedure Put_Extern_Decl
     (Rewriter     : Rewriter_T;
      Location     : Source_Location_T;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Func_Args    : String := "");
   --  Like Format_Extern_Decl, but write the definition at Location in the
   --  unit rewritten by Rewriter.

   procedure Put_Extern_Decl
     (File         : in out Text_Files.File_Type;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Func_Args    : String := "");
   --  Like Format_Extern_Decl, but write the definition to File

   function Find_First_Insert_Location
     (TU : Translation_Unit_T) return Source_Location_T;
   --  Find the first rewritable (raw) location of the file

   ------------------------
   -- To_Chars_Ptr_Array --
   ------------------------

   function To_Chars_Ptr_Array
     (V : String_Vectors.Vector) return chars_ptr_array is
   begin
      return Result : chars_ptr_array (1 .. Interfaces.C.size_t (V.Length)) do
         for I in Result'Range loop
            Result (I) := New_String (+V (Integer (I) - 1));
         end loop;
      end return;
   end To_Chars_Ptr_Array;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out chars_ptr_array) is
   begin
      for Item of Self loop
         Free (Item);
      end loop;
   end Free;

   ---------------------
   -- Compiler_Driver --
   ---------------------

   function Compiler_Driver
     (Project  : GNATCOLL.Projects.Project_Type;
      Language : C_Family_Language) return String is
   begin
      return GNATCOLL.Projects.Attribute_Value
               (Project, GPR.Compiler_Driver_Attribute, Image (Language));
   end Compiler_Driver;

   ------------------------
   -- Filter_Annotations --
   ------------------------

   procedure Filter_Annotations is
      use ALI_Annotation_Maps;
      Cur     : Cursor := ALI_Annotations.First;
      Del_Cur : Cursor;
   begin
      while Has_Element (Cur) loop
         declare
            Sloc    : constant Source_Location := Key (Cur);
            LI      : constant Line_Info_Access := Get_Line (Sloc);
            Deleted : Boolean := False;
         begin
            --  Check that no SCOs on the line of the annotation intersect
            --  the annotation Sloc. If one does, discard the annotation and
            --  warn the user as this is not a supported use case.

            if LI /= null and then LI.SCOs /= null
            then
               for SCO of LI.SCOs.all loop
                  if Slocs.In_Range (Sloc, Sloc_Range (SCO)) then
                     Del_Cur := Cur;
                     Next (Cur);
                     ALI_Annotations.Delete (Del_Cur);
                     Warn ("Exemption annotation at " & Slocs.Image (Sloc)
                           & " intersects a coverage obligation ("
                           & Image (SCO, True) & "), ignoring it");
                     Deleted := True;
                     exit;
                  end if;
               end loop;
            end if;
            if not Deleted then
               Next (Cur);
            end if;
         end;
      end loop;
   end Filter_Annotations;

   -------------------
   -- Source_Suffix --
   -------------------

   function Source_Suffix
     (Instrumenter : C_Family_Instrumenter_Type'Class;
      Part         : GNATCOLL.Projects.Unit_Parts;
      Project      : GNATCOLL.Projects.Project_Type) return String
   is
      L    : constant C_Family_Language := Instrumenter.Language;
      Attr : constant GPR.Attribute_Pkg_String :=
        GPR.Build
          (Package_Name   => "Naming",
           Attribute_Name =>
             (case Part is
              when GPR.Unit_Body     => "Body_Suffix",
              when GPR.Unit_Spec     => "Spec_Suffix",
              when GPR.Unit_Separate => raise Program_Error));
   begin
      case Part is
         when GPR.Unit_Body =>
            return Project.Attribute_Value
              (Attribute => Attr,
               Index     => Image (L),
               Default   => (case L is
                             when C_Language   => ".c",
                             when CPP_Language => ".cpp"));

         when GPR.Unit_Spec =>
            return Project.Attribute_Value
              (Attribute => Attr,
               Index     => Image (L),
               Default   => (case L is
                             when C_Language   => ".h",
                             when CPP_Language => ".hh"));

         when GPR.Unit_Separate =>
            return (raise Program_Error);
      end case;
   end Source_Suffix;

   ----------------
   -- Append_SCO --
   ----------------

   overriding procedure Append_SCO
     (Pass               : Record_PP_Info_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Local_Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
      Loc  : Source_Location_T := Get_Range_Start (Get_Cursor_Extent (N));
      Info : PP_Info;
   begin
      Append_SCO
        (C1, C2, From, To, UIC.SFI, Last, Pragma_Aspect_Name);

      --  We add preprocessing information only for actual SCOs. Return there
      --  if this is an operator SCO.

      if C1 in '!' | '&' | '|' then
         return;
      end if;

      Info.Actual_Source_Range := (From, To);

      --  Check if this is comes from a macro expansion, in which case we need
      --  to record some information, for reporting purposes.

      if Is_Macro_Location (Loc) then
         declare
            Expansion_Stack : Expansion_Lists.List;
            Definition_Info : Expansion_Info;

            Macro_Expansion_Name    : US.Unbounded_String;
            Immediate_Expansion_Loc : Source_Location_T;
            Macro_Arg_Expanded_Loc  : Source_Location_T;
         begin
            --  Note: macro arguments are completely macro-expanded before they
            --  are substituted in a macro body, unless they are stringified or
            --  pasted with other tokens.
            --
            --  In the following example:
            --
            --  1: #define DECL(x) int x;
            --  2: #define ID(x) x
            --  3: ID(DECL(a));
            --
            --  The last (meaning at the bottom of the expansion stack) macro
            --  expansion will be the expansion of ID. If we go through the
            --  expansion locations, going up a macro caller each time and
            --  starting from the bottom of the stack, we will then get:
            --
            --  #define ID(x) x
            --                ^ note: from definition of macro ID at 1:15
            --
            --  ID(DECL(a));
            --  ^~ note: from expansion of macro ID at 3:1
            --
            --  ID(DECL(a));
            --     ^~~~~~~ note: from expansion of macro DECL at 3:4
            --
            --  Start by recording the location of the coverage obligation
            --  first token. This will give the user feedback on where the
            --  coverage obligation is located in the last macro expansion
            --  definition (which expands into the coverage obligation).
            --
            --  Note that we deal differently when the coverage obligation is
            --  in a macro argument expansion, and when it is not.
            --
            --  In the simple case of a no argument macro expansion:
            --
            --  #define DECL_X int x;
            --  DECL_X;
            --
            --  We want the macro definition information to be located at:
            --  #define DECL_X int x;
            --                 ^~~~~~
            --  This corresponds in clang terminology to the spelling location:
            --  this is where the actual tokens are.
            --
            --  In the more complex case of a coverage obligation inside a
            --  macro argument expansion:
            --
            --  1: #define ID(x) x
            --  2: ID(int x);
            --
            --  We want the macro definition information to be located at:
            --  #define ID(x) x
            --                ^
            --  This is not where the actual tokens for the coverage obligation
            --  lie, but it is where they will ultimately be expanded.
            --
            --  We can't use the same mechanism, and retrieve the spelling
            --  location, as it will point where the actual tokens are, i.e.
            --  at:
            --  ID(int x);
            --     ^~~~~~
            --  So we have to get the macro argument expansion location, and
            --  get its spelling location.

            if Is_Macro_Arg_Expansion (Loc, Macro_Arg_Expanded_Loc, UIC.TU)
            then
               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics
                    (Macro_Arg_Expanded_Loc, UIC.TU);

               Definition_Info :=
                 (Macro_Name => Macro_Expansion_Name,
                  Sloc       => Spelling_Location (Macro_Arg_Expanded_Loc));
            else
               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics (Loc, UIC.TU);
               Definition_Info :=
                 (Macro_Name => Macro_Expansion_Name,
                  Sloc       => Spelling_Location (Loc));
            end if;

            while Is_Macro_Location (Loc) loop

               Immediate_Expansion_Loc := Loc;

               --  Find the location of the immediately expanded macro. Getting
               --  the immediate expansion location yields a location in the
               --  definition of the immediately expanded macro (i.e. the last
               --  expanded macro to get the declaration).
               --
               --  In the following example:
               --
               --  1: #define DECL(x) int x;
               --  2: #define ID(x) x
               --  3: ID(DECL(a));
               --
               --  Getting the immediate expansion location of the preprocessed
               --  declaration will yield:
               --  1: #define ID(x) x;
               --                   ^L1: immediate expansion location of the
               --                    declaration location.
               --
               --  To get to the expansion point, we have to go up one level,
               --  and get the spelling location from there.
               --
               --  3: ID(DECL(a));
               --     ^L2: immediate expansion location of L1
               --
               --  Going up another level will yield:
               --
               --  3: ID(DECL(a));
               --        ^L3: immediate expansion location
               --
               --  Note that this is a bit counter-intuitive because of the
               --  arguments prescan (macro arguments being expanded before
               --  they are substituted in the macro body).
               --
               --  The logic implemented here is similar to the logic of the
               --  implementation of Get_Immediate_Macro_Name_For_Diagnostics
               --  as implemented in clang.

               while Is_Macro_Arg_Expansion
                 (Immediate_Expansion_Loc, Macro_Arg_Expanded_Loc, UIC.TU)
               loop
                  --  TODO??? Document why it is needed to loop while we are
                  --  in a macro argument expansion (did not manage to make an
                  --  example that looped several times). Note that this
                  --  strictly follows the implementation of
                  --  Get_Immediate_Macro_Name_For_Diagnostics implemented in
                  --  clang.

                  Immediate_Expansion_Loc :=
                    Get_Immediate_Expansion_Loc
                      (Immediate_Expansion_Loc, UIC.TU);
               end loop;

               --  Immediate_Expansion_Loc is the location of the token in the
               --  immediate expanded macro definition. To get to the expansion
               --  point, go up one level.

               Immediate_Expansion_Loc :=
                 Get_Immediate_Expansion_Loc (Immediate_Expansion_Loc, UIC.TU);
               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics (Loc, UIC.TU);

               --  Then, keep going up the expansion stack

               Loc := Get_Immediate_Macro_Caller_Loc (Loc, UIC.TU);

               --  If the returned Macro_Definition_Name is an empty string,
               --  then it means the location refers to a token paste, or
               --  stringization and not a macro at all. Let's walk past it.

               if Length (Macro_Expansion_Name) /= 0 then
                  Expansion_Stack.Append
                    ((Macro_Name => Macro_Expansion_Name,
                      Sloc       => Spelling_Location
                                      (Immediate_Expansion_Loc)));
               end if;
            end loop;

            Info :=
              (Kind                => In_Expansion,
               PP_Source_Range     => Info.PP_Source_Range,
               Actual_Source_Range => Info.Actual_Source_Range,
               Expansion_Stack     => Expansion_Stack,
               Definition_Loc      => Definition_Info);
         end;
      end if;

      UIC.LL_PP_Info_Map.Insert (SCOs.SCO_Table.Last, Info);
   end Append_SCO;

   overriding procedure Append_SCO
     (Pass               : Instrument_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Local_Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
   begin
      Append_SCO
        (C1, C2, From, To, UIC.SFI, Last, Pragma_Aspect_Name);

      --  If this SCO is in a macro expansion, let's add source location
      --  information: we want to be able to know the actual source location
      --  of the SCO in the preprocessed code. This will allow us to
      --  retrieve the actual string (from the preprocessed code) when
      --  producing a coverage report.

      if UIC.LL_PP_Info_Map.Contains (SCOs.SCO_Table.Last) then
         declare
            Cursor_Source_Range_C : constant Source_Range_T :=
              Get_Cursor_Extent (N);
            Start_Loc             : constant Source_Location_T :=
              Get_Range_Start (Cursor_Source_Range_C);
            End_Loc               : constant Source_Location_T :=
              Get_Range_End (Cursor_Source_Range_C);

            Cursor_Source_Range : Slocs.Local_Source_Location_Range;

            procedure Update (LL_SCO : Nat; Info : in out PP_Info);

            ------------
            -- Update --
            ------------

            procedure Update (LL_SCO : Nat; Info : in out PP_Info) is
               pragma Unreferenced (LL_SCO);
            begin
               if Info.Kind = In_Expansion then
                  Info.PP_Source_Range := Cursor_Source_Range;
               end if;
            end Update;

         begin
            --  Get start and end of the range. Note: End_Loc is exclusive,
            --  whereas we need Cursor_Source_Range.Last_Sloc to be inclusive.

            Cursor_Source_Range.First_Sloc := File_Location (Start_Loc);
            Cursor_Source_Range.Last_Sloc := File_Location (End_Loc);
            Cursor_Source_Range.Last_Sloc.Column :=
              Cursor_Source_Range.Last_Sloc.Column - 1;

            UIC.LL_PP_Info_Map.Update_Element
              (UIC.LL_PP_Info_Map.Find (SCOs.SCO_Table.Last), Update'Access);
         end;
      end if;
   end Append_SCO;

   -------------
   -- Curlify --
   -------------

   overriding procedure Curlify
     (Pass : Instrument_Pass_Kind; N : Cursor_T; Rew : Rewriter_T) is
   begin
      Curlify (N, Rew);
   end Curlify;

   --------------------------------
   -- Find_Instrumented_Entities --
   --------------------------------

   function Find_Instrumented_Entities
     (UIC : in out C_Unit_Inst_Context'Class;
      SFI : Valid_Source_File_Index)
      return C_Instrumented_Entities_Maps.Reference_Type
   is
      use C_Instrumented_Entities_Maps;
      Cur   : Cursor;
      Dummy : Boolean;
   begin
      UIC.Instrumented_Entities.Insert
        (SFI, (others => <>), Cur, Dummy);
      return UIC.Instrumented_Entities.Reference (Cur);
   end Find_Instrumented_Entities;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   overriding procedure Insert_MCDC_State
     (Pass       : Instrument_Pass_Kind;
      UIC        : in out C_Unit_Inst_Context'Class;
      Name       : String;
      MCDC_State : out US.Unbounded_String) is
   begin
      MCDC_State := +Insert_MCDC_State (UIC, Name);
   end Insert_MCDC_State;

   --------------------------
   -- Instrument_Statement --
   --------------------------

   overriding procedure Instrument_Statement
     (Pass         : Instrument_Pass_Kind;
      UIC          : in out C_Unit_Inst_Context'Class;
      LL_SCO       : Nat;
      Insertion_N  : Cursor_T;
      Instr_Scheme : Instr_Scheme_Type) is
   begin
      UIC.Find_Instrumented_Entities (Last_File).Statements.Append
        (C_Source_Statement'
           (LL_SCO       => SCOs.SCO_Table.Last,
            Instr_Scheme => Instr_Scheme,
            Statement    => Insertion_N));
   end Instrument_Statement;

   -------------------------
   -- Instrument_Decision --
   -------------------------

   overriding procedure Instrument_Decision
     (Pass     : Instrument_Pass_Kind;
      UIC      : in out C_Unit_Inst_Context'Class;
      LL_SCO   : Nat;
      Decision : Cursor_T;
      State    : US.Unbounded_String) is
   begin
      UIC.Find_Instrumented_Entities (Last_File).Decisions.Append
        (C_Source_Decision'
           (LL_SCO   => LL_SCO,
            Decision => Decision,
            State    => State));
   end Instrument_Decision;

   --------------------------
   -- Instrument_Condition --
   --------------------------

   overriding procedure Instrument_Condition
     (Pass      : Instrument_Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : US.Unbounded_String;
      First     : Boolean) is
   begin
      UIC.Find_Instrumented_Entities (Last_File).Conditions.Append
        (C_Source_Condition'
           (LL_SCO    => SCOs.SCO_Table.Last,
            Condition => Condition,
            State     => State,
            First     => First));
   end Instrument_Condition;

   -----------------------
   -- Make_Expr_Witness --
   -----------------------

   function Make_Expr_Witness
     (UIC          : C_Unit_Inst_Context;
      Buffer_Index : Positive;
      Bit          : Bit_Id) return String is
   begin
      return
        "gnatcov_rts_witness ("
        & Statement_Buffer_Symbol (UIC.Instrumented_Unit)
        & Buffers_Subscript (Buffer_Index) & ", " & Img (Bit) & ")";
   end Make_Expr_Witness;

   ----------------------------
   -- Make_Statement_Witness --
   ----------------------------

   function Make_Statement_Witness
     (UIC          : C_Unit_Inst_Context;
      Buffer_Index : Positive;
      Bit          : Bit_Id) return String is
   begin
      return Make_Expr_Witness (UIC, Buffer_Index, Bit) & ";";
   end Make_Statement_Witness;

   ------------------------------
   -- Insert_Statement_Witness --
   ------------------------------

   procedure Insert_Statement_Witness
     (UIC           : in out C_Unit_Inst_Context;
      Buffers_Index : Positive;
      SS            : C_Source_Statement)
   is
      Unit_Bits : Allocated_Bits renames UIC.Allocated_Bits (Buffers_Index);

      --  Allocate a bit in the statement coverage buffer

      Bit : constant Bit_Id :=
        Allocate_Statement_Bit (Unit_Bits, SS.LL_SCO);
   begin
      --  Insert the call to the witness function: as a foregoing statement if
      --  SS.Statement is a statement, or as a previous expression (using the
      --  comma operator) if SS.Statement is an expression.

      case SS.Instr_Scheme is
         when Instr_Stmt =>
            Insert_Text_After_Start_Of
              (N    => SS.Statement,
               Text => Make_Statement_Witness (UIC, Buffers_Index, Bit),
               Rew  => UIC.Rewriter);

         when Instr_Expr =>
            Insert_Text_After_Start_Of
              (N    => SS.Statement,
               Text =>
                 "(" & Make_Expr_Witness (UIC, Buffers_Index, Bit) & ", ",
               Rew  => UIC.Rewriter);
            Insert_Text_Before_End_Of
              (N    => SS.Statement,
               Text => ")",
               Rew  => UIC.Rewriter);
      end case;
   end Insert_Statement_Witness;

   ------------------------------
   -- Insert_Condition_Witness --
   ------------------------------

   procedure Insert_Condition_Witness
     (UIC    : in out C_Unit_Inst_Context;
      SC     : C_Source_Condition;
      Offset : Natural) is
   begin
      --  No instrumentation for condition if there is no local state variable

      if US.Length (SC.State) = 0 then
         return;
      end if;

      declare
         First_Image : constant String :=
           Integer'Image (if SC.First then 1 else 0);
      begin
         --  Wrap the condition inside a ternary expression so that we always
         --  pass an unsigned value to the witness function. This turns <cond>
         --  into (<cond>) ? 1 : 0.

         Insert_Text_After_Start_Of
           (N    => SC.Condition,
            Text =>
              "gnatcov_rts_witness_condition ("
              & US.To_String (SC.State) & ", "
              & Img (Offset) & ", "
              & First_Image & ", "
              & "(",
            Rew  => UIC.Rewriter);
         Insert_Text_Before_End_Of (N    => SC.Condition,
                                    Text => ") ? 1 : 0)",
                                    Rew  => UIC.Rewriter);
      end;
   end Insert_Condition_Witness;

   -----------------------------
   -- Insert_Decision_Witness --
   -----------------------------

   procedure Insert_Decision_Witness
     (UIC           : in out C_Unit_Inst_Context;
      Buffers_Index : Positive;
      SD            : C_Source_Decision;
      Path_Count    : Natural)
   is
      Unit_Bits : Allocated_Bits renames UIC.Allocated_Bits (Buffers_Index);

      N : Cursor_T renames SD.Decision;

      --  Allocate bits for this decision in coverage buffers

      Bits : constant Decision_Bit_Ids :=
        Allocate_Decision_Bits
          (Unit_Bits,
           (Unit_Bits.SFI, Start_Sloc (N)),
           SD.LL_SCO,
           SD.State,
           Path_Count);
   begin
      --  Now attach witness call at the place of the original decision

      declare
         Is_MCDC       : constant Boolean := Bits.Path_Bits_Base /= No_Bit_Id;
         Function_Name : constant String :=
           (if Is_MCDC
            then "gnatcov_rts_witness_decision_mcdc"
            else "gnatcov_rts_witness_decision");
      begin
         Insert_Text_After_Start_Of
           (N    => N,
            Text => Function_Name & "("
                    & Decision_Buffer_Symbol (UIC.Instrumented_Unit)
                    & Buffers_Subscript (Buffers_Index) & ", "
                    & Img (Bits.Outcome_Bits (False)) & ", "
                    & Img (Bits.Outcome_Bits (True)),
            Rew  => UIC.Rewriter);

         if Is_MCDC then
            Insert_Text_After_Start_Of
              (N    => N,
               Text => ", " & MCDC_Buffer_Symbol (UIC.Instrumented_Unit)
                       & Buffers_Subscript (Buffers_Index) & ", "
                       & Img (Bits.Path_Bits_Base) & ", "
                       & US.To_String (SD.State),
               Rew  => UIC.Rewriter);
         end if;
         Insert_Text_After_Start_Of (N    => N,
                                     Text => ", (",
                                     Rew  => UIC.Rewriter);

         --  Wrap the decision inside a ternary expression so that we always
         --  pass an unsigned value to the witness function. This turns <dec>
         --  into (<dec>) ? 1 : 0.

         Insert_Text_Before_End_Of (N    => N,
                                    Text => ") ? 1 : 0)",
                                    Rew  => UIC.Rewriter);
      end;
   end Insert_Decision_Witness;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   function Insert_MCDC_State
     (UIC : in out C_Unit_Inst_Context'Class; Name : String) return String
   is
      Var_Decl_Img  : constant String :=
        "unsigned " & Name & "_var;";
      Addr_Decl_Img : constant String :=
        "unsigned *" & Name & " = &" & Name & "_var;";

   begin
      Insert_Text_Before_Start_Of (N    => UIC.MCDC_State_Declaration_Node,
                                   Text => Var_Decl_Img & Addr_Decl_Img,
                                   Rew  => UIC.Rewriter);
      return Name;
   end Insert_MCDC_State;

   type SC_Entry is record
      N : Cursor_T;
      --  Original statement node, used to compute macro expansion information
      --  related to this SCO.

      Insertion_N : Cursor_T;
      --  If not null, node where the witness call should be inserted.
      --  Otherwise, the insertion node will be N.

      From : Local_Source_Location;
      To   : Local_Source_Location;
      Typ  : Character;

      Instr_Scheme : Instr_Scheme_Type := Instr_Stmt;
   end record;

   package SC is new Table.Table
     (Table_Component_Type => SC_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 200,
      Table_Name           => "SCO_SC");
   --  Used to store statement components for a CS entry to be output as a
   --  result of the call to this procedure. SC.Last is the last entry stored,
   --  so the current statement sequence is represented by SC_Array (SC_First
   --  .. SC.Last), where SC_First is saved on entry to each recursive call to
   --  the routine.
   --
   --  Extend_Statement_Sequence adds an entry to this array, and then
   --  Set_Statement_Entry clears the entries starting with SC_First, copying
   --  these entries to the main SCO output table. The reason that we do the
   --  temporary caching of results in this array is that we want the SCO table
   --  entries for a given CS line to be contiguous, and the processing may
   --  output intermediate entries such as decision entries.

   type SD_Entry is record
      Nod : Cursor_T;
      Typ : Character;
   end record;
   --  Used to store a single entry in the following table. Nod is the node to
   --  be searched for decisions for the case of Process_Decisions_Defer with a
   --  node argument (with Lst set to No_Ada_Node. Lst is the list to be
   --  searched for decisions for the case of Process_Decisions_Defer with a
   --  List argument (in which case Nod is set to No_Ada_Node).

   package SD is new Table.Table
     (Table_Component_Type => SD_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 200,
      Table_Name           => "SCO_SD");
   --  Used to store possible decision information. Instead of calling the
   --  Process_Decisions procedures directly, we call Process_Decisions_Defer,
   --  which simply stores the arguments in this table. Then when we clear
   --  out a statement sequence using Set_Statement_Entry, after generating
   --  the CS lines for the statements, the entries in this table result in
   --  calls to Process_Decision. The reason for doing things this way is to
   --  ensure that decisions are output after the CS line for the statements
   --  in which the decisions occur.
   --

   -------------------------
   -- Traverse_Statements --
   -------------------------

   procedure Traverse_Statements
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector);
   --  Process L, a list of statements or declarations

   procedure Traverse_Declarations
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector);
   --  Traverse a translation unit (top level declarations)

   procedure Process_Decisions
     (UIC : in out C_Unit_Inst_Context; N : Cursor_T; T : Character);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains.

   --------------------------
   -- Internal Subprograms --
   --------------------------

   function Has_Decision (T : Cursor_T) return Boolean;
   --  T is the node for a subtree. Returns True if any (sub)expression in T
   --  contains a nested decision (i.e. either is a logical operator, or
   --  contains a logical operator in its subtree).

   function Is_Logical_Operator (N : Cursor_T) return Boolean;
   --  Return whether N is an operator that can be part of a decision (! or
   --  && / ||).

   function Is_Complex_Decision (N : Cursor_T) return Boolean;
   --  Return whether N is a complex decision, i.e. a tree of ! / &&/ ||
   --  operators that contains at least one && or || operator.

   -------------------------
   -- Is_Complex_Decision --
   -------------------------

   function Is_Complex_Decision (N : Cursor_T) return Boolean is
      Opcode : constant String := Get_Opcode_Str (N);
   begin
      if Opcode = "" then
         return False;
      elsif Opcode = "!" then
         return Is_Complex_Decision (Get_Sub_Expr (N));
      elsif Opcode = "||" or else Opcode = "&&" then
         return True;
      else
         return False;
      end if;
   end Is_Complex_Decision;

   -----------------------
   -- Process_Decisions --
   -----------------------

   procedure Process_Decisions
     (UIC : in out C_Unit_Inst_Context; N : Cursor_T; T : Character)
   is
      Mark : Nat;
      --  This is used to mark the location of a decision sequence in the SCO
      --  table. We use it for backing out a simple decision in an expression
      --  context that contains only NOT operators.

      Mark_Hash : Nat;
      --  Likewise for the putative SCO_Raw_Hash_Table entries: see below

      type Hash_Entry is record
         Sloc      : Local_Source_Location;
         SCO_Index : Nat;
      end record;
      --  We must register all conditions/pragmas in SCO_Raw_Hash_Table.
      --  However we cannot register them at the same time we are adding the
      --  corresponding SCO entries to the raw table since we may discard them
      --  later on. So instead we put all putative conditions into Hash_Entries
      --  (see below) and register them once we are sure we keep them.
      --
      --  This data structure holds the conditions/pragmas to register in
      --  SCO_Raw_Hash_Table.

      package Hash_Entries is new Table.Table
        (Table_Component_Type => Hash_Entry,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 10,
         Table_Name           => "Hash_Entries");
      --  Hold temporarily (i.e. free'd before returning) the Hash_Entry before
      --  they are registered in SCO_Raw_Hash_Table.

      ---------------------------------
      -- Decision-specific variables --
      ---------------------------------

      --  The following variables are related to the current decision being
      --  processed by this call to Process_Decisions. Note that in the case
      --  of nested decisions, this subprogram recurses, so we do not have to
      --  worry about overwriting them.

      Current_Decision : Nat;
      --  Low level SCO id of current decision

      X_Not_Decision : Boolean;
      --  This flag keeps track of whether a decision sequence in the SCO table
      --  contains only NOT operators, and is for an expression context (T=X).
      --  The flag will be set False if T is other than X, or if an operator
      --  other than NOT is in the sequence.

      Condition_Count : Natural := 0;
      --  Count of conditions for current decision (MC/DC only)

      MCDC_State : US.Unbounded_String;
      --  Name of MC/DC state local variable for current decision (MC/DC only)

      procedure Output_Decision_Operand (Operand : Cursor_T);
      --  The node Operand is the top level logical operator of a decision, or
      --  it is one of the operands of a logical operator belonging to a single
      --  complex decision. This (recursive) routine outputs the sequence of
      --  table entries corresponding to the node. Note that we do not process
      --  the sub- operands to look for further decisions, that processing is
      --  done in Find_Nested_Decisions, because we can't get decisions mixed
      --  up in the global table. Call has no effect if Operand is Empty.
      --  Increments Condition_Count (recursively) for each condition.

      procedure Output_Element (N : Cursor_T);
      --  Node N is an operand of a logical operator that is not itself a
      --  logical operator, or it is a simple decision. This routine outputs
      --  the table entry for the element, with C1 set to ' '. Last is set
      --  False, and an entry is made in the condition hash table.

      procedure Output_Header (T : Character; N : Cursor_T);
      --  Outputs a decision header node. T is I/W/E/P for IF/WHILE/EXIT WHEN/
      --  PRAGMA, and 'X' for the expression case. Resets Condition_Count to 0,
      --  and initializes MCDC_State.

      procedure Find_Nested_Decisions (Operand : Cursor_T);
      --  This is called on node Operand, the top level node of a decision,
      --  or on one of its operands or suboperands after generating the full
      --  output for the complex decision. It process the suboperands of the
      --  decision looking for nested decisions.

      function Process_Node (N : Cursor_T) return Child_Visit_Result_T;
      --  Processes one node in the traversal, looking for logical operators,
      --  and if one is found, outputs the appropriate table entries.

      -----------------------------
      -- Output_Decision_Operand --
      -----------------------------

      procedure Output_Decision_Operand (Operand : Cursor_T) is
         C1 : Character;
         C2 : Character;
         --  C1 holds a character that identifies the operation while C2
         --  indicates whether we are sure (' ') or not ('?') this operation
         --  belongs to the decision. '?' entries will be filtered out in the
         --  second (SCO_Record_Filtered) pass.

         N : constant Cursor_T := Unwrap (Operand);

         L, R : Cursor_T;

         Op_N : constant String := Get_Opcode_Str (N);

      begin

         --  Logical operator

         if Is_Logical_Operator (N) then
            if Op_N = "!" then
               C1 := '!';
               L := Get_Null_Cursor;
               R := Get_Sub_Expr (N);

            else
               --  N is a binary logical operator

               L := Get_LHS (N);
               R := Get_RHS (N);
               if Op_N = "||" then
                  C1 := '|';
               else
                  pragma Assert (Op_N = "&&");
                  C1 := '&';
               end if;
            end if;

            C2 := ' ';

            UIC.Pass.Append_SCO
              (UIC  => UIC,
               N    => N,
               C1   => C1,
               C2   => C2,
               From => Sloc (Get_Operator_Loc (N)),
               To   => No_Local_Location,
               Last => False);

            Hash_Entries.Append ((Sloc      => Start_Sloc (N),
                                  SCO_Index => SCOs.SCO_Table.Last));

            if not Is_Null (L) then
               Output_Decision_Operand (L);
            end if;
            Output_Decision_Operand (R);

         --  Not a logical operator -> condition

         else
            Output_Element (N);

            if MCDC_Coverage_Enabled then
               UIC.Pass.Instrument_Condition
                 (UIC       => UIC,
                  LL_SCO    => SCOs.SCO_Table.Last,
                  Condition => N,
                  State     => MCDC_State,
                  First     => Condition_Count = 0);

               Condition_Count := Condition_Count + 1;
            end if;
         end if;
      end Output_Decision_Operand;

      --------------------
      -- Output_Element --
      --------------------

      procedure Output_Element (N : Cursor_T) is
      begin
         UIC.Pass.Append_SCO
           (UIC  => UIC,
            N    => N,
            C1   => ' ',
            C2   => 'c',
            From => Start_Sloc (N),
            To   => End_Sloc (N),
            Last => False);
         Hash_Entries.Append ((Start_Sloc (N), SCOs.SCO_Table.Last));
      end Output_Element;

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header (T : Character; N : Cursor_T) is
      begin
         UIC.Pass.Append_SCO
           (UIC  => UIC,
            N    => N,
            C1   => T,
            C2   => ' ',
            From => Start_Sloc (N),
            To   => End_Sloc (N),
            Last => False);

         Current_Decision := SCOs.SCO_Table.Last;

         if Coverage.Enabled (Coverage_Options.Decision)
           or else MCDC_Coverage_Enabled
         then
            if MCDC_Coverage_Enabled then
               Condition_Count := 0;

               UIC.Pass.Insert_MCDC_State
                 (UIC, Make_MCDC_State_Name (SCOs.SCO_Table.Last), MCDC_State);
            end if;

            UIC.Pass.Instrument_Decision
              (UIC      => UIC,
               LL_SCO   => Current_Decision,
               Decision => N,
               State    => MCDC_State);
         end if;

      end Output_Header;

      ---------------------------
      -- Find_Nested_Decisions --
      ---------------------------

      procedure Find_Nested_Decisions (Operand : Cursor_T) is
         N : constant Cursor_T := Unwrap (Operand);
      begin
         if Is_Logical_Operator (N) then

            if Kind (N) = Cursor_Unary_Operator then
               Find_Nested_Decisions (Get_Sub_Expr (N));

            else
               Find_Nested_Decisions (Get_LHS (N));
               Find_Nested_Decisions (Get_RHS (N));
               X_Not_Decision := False;
            end if;

         else
            Process_Decisions (UIC, N, 'X');
         end if;
      end Find_Nested_Decisions;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : Cursor_T) return Child_Visit_Result_T is
         --  Test for the two cases where N is the root node of some decision:

         Decision_Root : constant Boolean :=

           --  Simple decision at outer level: a boolean expression (which is
           --  not a logical operator) appearing as the operand of an IF,
           --  WHILE, FOR construct.

           (N = Process_Decisions.N and then T /= 'X')
             or else

           --  Complex decision, whether at outer level or nested: a boolean
           --  expression involving a logical operator.

           Is_Complex_Decision (N);

      begin
         if Decision_Root then
            declare
               T  : Character;

            begin
               --  If outer level, then type comes from call, otherwise it
               --  is more deeply nested and counts as X for expression.

               if N = Process_Decisions.N then
                  T := Process_Decisions.T;
               else
                  T := 'X';
               end if;

               --  Output header for sequence

               X_Not_Decision := T = 'X' and then Get_Opcode_Str (N) = "!";
               Mark      := SCOs.SCO_Table.Last;
               Mark_Hash := Hash_Entries.Last;
               Output_Header (T, N);

               --  Output the decision (recursively traversing operands)

               Output_Decision_Operand (N);

               --  If the decision was in an expression context (T = 'X')
               --  and contained only NOT operators, then we do not output
               --  it, so delete the associated SCO entries. As a consequence,
               --  no instrumentation will be emitted.

               if X_Not_Decision then
                  SCOs.SCO_Table.Set_Last (Mark);
                  Hash_Entries.Set_Last (Mark_Hash);

               --  Otherwise, set Last in last table entry to mark end

               else
                  SCOs.SCO_Table.Table (SCOs.SCO_Table.Last).Last := True;
               end if;

               --  Process any embedded decisions

               Find_Nested_Decisions (N);
               return Child_Visit_Continue;
            end;
         end if;

         case Kind (N) is
            when Cursor_Conditional_Operator =>
               declare
                  Cond       : constant Cursor_T := Get_Cond (N);
                  True_Expr  : constant Cursor_T := Get_LHS (N);
                  False_Expr : constant Cursor_T := Get_RHS (N);
               begin
                  Process_Decisions (UIC, Cond, 'I');
                  Process_Decisions (UIC, True_Expr, 'X');
                  Process_Decisions (UIC, False_Expr, 'X');
                  return Child_Visit_Continue;
               end;
            when others =>
               null;
         end case;

         return Child_Visit_Recurse;
      end Process_Node;

   --  Start of processing for Process_Decisions

   begin
      if Is_Null (N) then
         return;
      end if;
      Hash_Entries.Init;
      Visit (N, Process_Node'Access);
      Hash_Entries.Free;
   end Process_Decisions;

   procedure Process_Decisions_Defer (N : Cursor_T; T : Character)
   with Inline;
   --  This routine is logically the same as Process_Decisions, except that the
   --  arguments are saved in the SD table for later processing when
   --  Set_Statement_Entry is called, which goes through the saved entries
   --  making the corresponding calls to Process_Decision. Note: the enclosing
   --  statement must have already been added to the current statement
   --  sequence, so that nested decisions are properly identified as such.

   -----------------------------
   -- Process_Decisions_Defer --
   -----------------------------

   procedure Process_Decisions_Defer (N : Cursor_T; T : Character) is
   begin
      SD.Append ((N, T));
   end Process_Decisions_Defer;

   ------------------
   -- Has_Decision --
   ------------------

   function Has_Decision (T : Cursor_T) return Boolean is

      function Visitor (N : Cursor_T) return Child_Visit_Result_T;
      --  If N's kind indicates the presence of a decision, return
      --  Child_Visit_Break, otherwise return Child_Visit_Recurse.
      --
      --  We know have a decision as soon as we have a logical operator (by
      --  definition).

      Has_Decision : Boolean := False;

      -----------
      -- Visit --
      -----------

      function Visitor (N : Cursor_T) return Child_Visit_Result_T
      is
      begin
         if (Is_Expression (Kind (N)) and then Is_Complex_Decision (N))
             or else Kind (N) = Cursor_Conditional_Operator
         then
            Has_Decision := True;
            return Child_Visit_Break;

         --  We don't want to visit lambda expressions: we will treat them
         --  outside of the current expression.

         elsif Kind (N) = Cursor_Lambda_Expr then
            return Child_Visit_Continue;
         else
            return Child_Visit_Recurse;
         end if;
      end Visitor;

   --  Start of processing for Has_Decision

   begin
      Visit (T, Visitor'Access);
      return Has_Decision;
   end Has_Decision;

   -------------------------
   -- Is_Logical_Operator --
   -------------------------

   function Is_Logical_Operator (N : Cursor_T) return Boolean is
      Opcode : constant String := Get_Opcode_Str (N);
   begin
      return (Opcode = "!" or else Opcode = "&&" or else Opcode = "||");
   end Is_Logical_Operator;

   ---------------------
   -- Run_Diagnostics --
   ---------------------

   procedure Run_Diagnostics (TU : Translation_Unit_T) is
      Num_Diag : constant unsigned := Get_Num_Diagnostics (TU);
   begin
      for I in 1 .. Num_Diag loop
         declare
            Diag     : constant Diagnostic_T :=
              Get_Diagnostic (Unit => TU, Index => I - 1);
            Severity : constant Diagnostic_Severity_T :=
              Get_Diagnostic_Severity (Diag);
            Str      : constant String :=
              Format_Diagnostic
                (Diagnostic => Diag,
                 Options    => Default_Diagnostic_Display_Options);
         begin
            case Severity is
               when Diagnostic_Error | Diagnostic_Fatal =>
                  Outputs.Error ("Error when parsing the file " & Str);
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Run_Diagnostics;

   -------------------------
   -- Traverse_Statements --
   -------------------------

   procedure Traverse_Statements
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector)
   is
      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;

      procedure Traverse_One (N : Cursor_T);
      --  Traverse a statement

      procedure Extend_Statement_Sequence
        (N            : Cursor_T;
         Typ          : Character;
         Insertion_N  : Cursor_T := Get_Null_Cursor;
         Instr_Scheme : Instr_Scheme_Type := Instr_Stmt);
      --  Add an entry to the SC table

      procedure Set_Statement_Entry;
      --  Output CS entries for all statements saved in table SC, and end the
      --  current CS sequence. Then output entries for all decisions nested in
      --  these statements, which have been deferred so far.

      ------------------
      -- Traverse_One --
      ------------------

      procedure Traverse_One (N : Cursor_T) is
         use Cursor_Vectors;
      begin
         --  Initialize or extend current statement sequence. Note that for
         --  special cases such as IF and SWITCH statements we will modify
         --  the range to exclude internal statements that should not be
         --  counted as part of the current statement sequence.
         case Kind (N) is

            --  Label, which breaks the current statement sequence, but the
            --  label itself is not included in the next statement sequence,
            --  since it generates no code.

            when Cursor_Label_Ref =>
               Set_Statement_Entry;

            --  Compound statement, which breaks the current statement sequence

            when Cursor_Compound_Stmt =>
               Set_Statement_Entry;
               Traverse_Statements (IC, UIC, L => Get_Children (N));

            --  If statement, which breaks the current statement sequence, but
            --  we include the condition in the current sequence.

            when Cursor_If_Stmt =>
               Extend_Statement_Sequence (N, 'I');

               declare
                  Then_Part : constant Cursor_T := Get_Then (N);
                  Else_Part : constant Cursor_T := Get_Else (N);
               begin
                  Process_Decisions_Defer (Get_Cond (N), 'I');
                  Set_Statement_Entry;

                  --  Now we traverse the statements in the THEN part

                  UIC.Pass.Curlify (N   => Then_Part,
                                    Rew => UIC.Rewriter);
                  Traverse_Statements
                    (IC, UIC,
                     L => To_Vector (Then_Part));

                  --  Traverse the ELSE statements if present

                  if not Is_Null (Else_Part) then
                     UIC.Pass.Curlify (N   => Else_Part,
                                       Rew => UIC.Rewriter);
                     Traverse_Statements
                       (IC, UIC,
                        L => To_Vector (Else_Part));
                  end if;
               end;

            --  Switch statement, which breaks the current statement sequence,
            --  but we include the expression in the current sequence.

            when Cursor_Switch_Stmt =>
               Extend_Statement_Sequence (N, 'C');
               declare
                  Switch_Cond : constant Cursor_T := Get_Cond (N);
                  Alt         : constant Cursor_T := Get_Body (N);
               begin
                  Process_Decisions_Defer (Switch_Cond, 'X');
                  Set_Statement_Entry;

                  --  Process case branches

                  Traverse_Statements (IC, UIC, L => To_Vector (Alt));
               end;

            --  Case alternative

            when Cursor_Case_Stmt | Cursor_Default_Stmt =>
               declare
                  Case_Body : constant Cursor_T := Get_Sub_Stmt (N);
               begin
                  Traverse_Statements (IC, UIC, L => To_Vector (Case_Body));
               end;

            --  Loop ends the current statement sequence, but we include
            --  the iteration scheme if present in the current sequence.
            --  But the body of the loop starts a new sequence, since it
            --  may not be executed as part of the current sequence.

            when Cursor_While_Stmt =>
               declare
                  While_Body : constant Cursor_T := Get_Body (N);
                  Cond_Var   : constant Cursor_T := Get_Cond_Var (N);
                  Cond       : constant Cursor_T := Get_Cond (N);

               begin
                  UIC.Pass.Curlify (N   => While_Body,
                                    Rew => UIC.Rewriter);

                  --  If the loop condition is a declaration, instrument its
                  --  initialization expression.

                  Extend_Statement_Sequence
                    (N, 'W',
                     Insertion_N  => (if Is_Null (Cond_Var)
                                      then Cond
                                      else Get_Var_Init_Expr (Cond_Var)),
                     Instr_Scheme => Instr_Expr);

                  Process_Decisions_Defer (Cond, 'W');
                  Set_Statement_Entry;
                  Traverse_Statements (IC, UIC, To_Vector (While_Body));
               end;

            --  Do while statement. Ends the current statement sequence.

            when Cursor_Do_Stmt =>
               declare
                  Do_Body  : constant Cursor_T := Get_Body (N);
                  Do_While : constant Cursor_T := Get_Cond (N);

               begin
                  UIC.Pass.Curlify (N   => Do_Body,
                                    Rew => UIC.Rewriter);

                  Traverse_Statements (IC, UIC, To_Vector (Do_Body));
                  Extend_Statement_Sequence
                    (Do_While, 'W', Instr_Scheme => Instr_Expr);

                  Process_Decisions_Defer (Do_While, 'W');
                  Set_Statement_Entry;

               end;

            --  For statement. Ends the current statement sequence.

            when Cursor_For_Stmt =>
               declare
                  For_Init : constant Cursor_T := Get_For_Init (N);
                  For_Cond : constant Cursor_T := Get_Cond (N);
                  For_Inc  : constant Cursor_T := Get_For_Inc (N);
                  For_Body : constant Cursor_T := Get_Body (N);
               begin
                  Extend_Statement_Sequence
                    (For_Init, ' ', Insertion_N => N);
                  Extend_Statement_Sequence
                    (For_Cond, 'F', Instr_Scheme => Instr_Expr);

                  --  The guard expression for the FOR loop is a decision. The
                  --  closest match for this kind of decision is a while loop.

                  Process_Decisions_Defer (For_Cond, 'W');

                  Set_Statement_Entry;

                  UIC.Pass.Curlify (N => For_Body, Rew => UIC.Rewriter);
                  Traverse_Statements (IC, UIC, To_Vector (For_Body));

                  Extend_Statement_Sequence
                    (For_Inc, ' ', Instr_Scheme => Instr_Expr);

                  Set_Statement_Entry;
               end;

            when Cursor_CXX_For_Range_Stmt =>
               declare
                  For_Init_Stmt  : constant Cursor_T := Get_For_Init (N);
                  For_Range_Decl : constant Cursor_T := Get_For_Range_Expr (N);
                  For_Body       : constant Cursor_T := Get_Body (N);
               begin
                  --  Generate SCO statements for both the init statement and
                  --  the range declaration initialization expression. Like all
                  --  statements, they can contain nested decisions.

                  Extend_Statement_Sequence
                    (For_Init_Stmt, ' ', Insertion_N => N);
                  Process_Decisions_Defer (For_Init_Stmt, 'X');

                  Extend_Statement_Sequence
                    (For_Range_Decl, ' ',
                     Insertion_N  => For_Range_Decl,
                     Instr_Scheme => Instr_Expr);
                  Process_Decisions_Defer (For_Range_Decl, 'X');

                  Set_Statement_Entry;

                  --  Generate obligations for body statements

                  UIC.Pass.Curlify (N => For_Body, Rew => UIC.Rewriter);
                  Traverse_Statements (IC, UIC, To_Vector (For_Body));
               end;

           --  Unconditional goto, which is included in the current statement
           --  sequence, but then terminates it.

            when Cursor_Goto_Stmt | Cursor_Indirect_Goto_Stmt =>
               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;

            when Cursor_Label_Stmt =>
               Set_Statement_Entry;
               Traverse_Statements (IC, UIC, Get_Children (N));

            when Cursor_Stmt_Expr =>
               Traverse_Statements (IC, UIC, Get_Children (N));

            --  Null statement, we won't monitor their execution

            when Cursor_Null_Stmt =>
               null;

            --  TODO??? there are probably missing special statements, such as
            --  ternary operator etc. Do that in a later step.

            when others =>

               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               Extend_Statement_Sequence (N, ' ');

               --  Process any embedded decisions

               if Has_Decision (N) then
                  Process_Decisions_Defer (N, 'X');
               end if;
         end case;

         --  Traverse lambda expressions, if any

         Traverse_Declarations
           (IC  => IC,
            UIC => UIC,
            L   => Get_Lambda_Exprs (N));
      end Traverse_One;

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence
        (N            : Cursor_T;
         Typ          : Character;
         Insertion_N  : Cursor_T := Get_Null_Cursor;
         Instr_Scheme : Instr_Scheme_Type := Instr_Stmt)
      is
         Insert_Cursor : aliased Cursor_T := N;

         F : constant Local_Source_Location := Start_Sloc (N);
         T : Local_Source_Location := End_Sloc (N);
         --  Source location bounds used to produce a SCO statement. By
         --  default, this should cover the same source location range as N,
         --  however for nodes that can contain other statements, we select an
         --  end bound that appears before the first nested statement (see
         --  To_Node below).

         To_Node : Cursor_T := Get_Null_Cursor;
         --  In the case of simple statements, set to null cursor and unused.
         --  Otherwise, use F and this node's end sloc for the emitted
         --  statement source location range.
      begin
         --  For now, instrument only cursors that come from the file being
         --  instrumented, and do not instrument included code.

         if Is_Null (N) or else not Is_Source_Of_Interest (UIC, N) then
            return;
         end if;

         --  If N contains nested statements, set To_Node to the controlling
         --  expression, so that the sloc range for the SCO spans from the
         --  starting keyword to the end of the controlling expression.

         case Kind (N) is
            when Cursor_If_Stmt | Cursor_Switch_Stmt =>
               To_Node := Get_Cond (N);
            when Cursor_While_Stmt =>
               Insert_Cursor := Get_Cond (N);
               To_Node := Insert_Cursor;
            when others =>
               null;
         end case;

         if not Is_Null (To_Node) then
            T := End_Sloc (To_Node);
         end if;

         SC.Append
           ((N            => Insert_Cursor,
             Insertion_N  =>
                 (if Is_Null (Insertion_N)
                  then N
                  else Insertion_N),
             From         => F,
             To           => T,
             Typ          => Typ,
             Instr_Scheme => Instr_Scheme));
      end Extend_Statement_Sequence;

      -------------------------
      -- Set_Statement_Entry --
      -------------------------

      procedure Set_Statement_Entry is
         SC_Last : constant Types.Int := SC.Last;
         SD_Last : constant Types.Int := SD.Last;
      begin
         for J in SC_First .. SC_Last loop
            declare
               SCE : SC_Entry renames SC.Table (J);
            begin
               UIC.Pass.Append_SCO
                 (UIC  => UIC,
                  N    => SCE.N,
                  C1   => 'S',
                  C2   => SCE.Typ,
                  From => SCE.From,
                  To   => SCE.To,
                  Last => (J = SC_Last));

               UIC.Pass.Instrument_Statement
                 (UIC          => UIC,
                  LL_SCO       => SCOs.SCO_Table.Last,
                  Insertion_N  => SCE.Insertion_N,
                  Instr_Scheme => SCE.Instr_Scheme);
            end;
         end loop;

         --  Clear out used section of SC table

         SC.Set_Last (SC_First - 1);

         --  Output any embedded decisions

         for J in SD_First .. SD_Last loop
            declare
               SDE : SD_Entry renames SD.Table (J);

            begin
               Process_Decisions (UIC, SDE.Nod, SDE.Typ);
            end;
         end loop;

         --  Clear out used section of SD table

         SD.Set_Last (SD_First - 1);

      end Set_Statement_Entry;

      use Cursor_Vectors;

      Emit_SCOs : Boolean := False;

   --  Start of processing for Traverse_Statements

   begin
      for N of L loop
         Traverse_One (N);
         Emit_SCOs := True;
      end loop;

      if Emit_SCOs then
         Set_Statement_Entry;
      end if;
   end Traverse_Statements;

   ---------------------------
   -- Traverse_Declarations --
   ---------------------------

   procedure Traverse_Declarations
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector)
   is
      use Cursor_Vectors;
      Saved_MCDC_State_Declaration_Node : constant Cursor_T :=
        UIC.MCDC_State_Declaration_Node;
   begin
      for N of L loop

         --  Only traverse the function declarations that belong to a unit of
         --  interest.

         begin
            if Is_Source_Of_Interest (UIC, N) then

               case Kind (N) is

                  --  Traverse the statements of function bodies

                  when Cursor_Function_Decl
                     | Cursor_Function_Template
                     | Cursor_CXX_Method
                     | Cursor_Constructor
                     | Cursor_Destructor
                     | Cursor_Lambda_Expr =>
                     declare
                        --  Get_Body returns a Compound_Stmt, convert it to a
                        --  list of statements using the Get_Children utility.

                        Fun_Body : constant Cursor_Vectors.Vector :=
                          Get_Children (Get_Body (N));
                     begin
                        if Fun_Body.Length > 0 then
                           UIC.MCDC_State_Declaration_Node :=
                             Fun_Body.First_Element;
                           Traverse_Statements (IC, UIC, Fun_Body);
                        end if;
                     end;

                   --  Traverse the declarations of a namespace / linkage
                   --  specifier etc.

                  when Cursor_Namespace
                     | Cursor_Linkage_Spec
                     | Cursor_Class_Template
                     | Cursor_Class_Decl =>
                     Traverse_Declarations (IC, UIC, Get_Children (N));

                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;

      --  Restore previous MCDC_State insertion node: we can have lambda
      --  expressions inside functions, and we don't want to keep inserting
      --  MCDC state variables in the lambda after we finished its traversal.

      UIC.MCDC_State_Declaration_Node := Saved_MCDC_State_Declaration_Node;
   end Traverse_Declarations;

   --------------------
   -- Builtin_Macros --
   --------------------

   function Builtin_Macros
     (Lang, Compiler, Std, Output_Dir : String) return Macro_Vector_Cst_Access
   is
      use Ada.Characters.Handling;
      use Compiler_Macros_Maps;

      L      : constant String := To_Lower (Lang);
      Key    : constant Unbounded_String :=
        +Compiler & " -x " & L & " " & Std;
      Cur    : constant Cursor := Compiler_Macros.Find (Key);
      Result : Macro_Vector_Access;
   begin
      --  If we already computed builtin macros for Compiler, return the cached
      --  result. Compute it now otherwise.

      if Has_Element (Cur) then
         Result := Element (Cur);

      else
         Result := new Macro_Vectors.Vector;
         declare
            Args     : String_Vectors.Vector;
            Basename : constant String :=
              Ada.Directories.Simple_Name (Compiler) & "_builtins";
            Filename : constant String := Output_Dir / Basename;
            File     : Ada.Text_IO.File_Type;
         begin
            --  Run the preprocessor on an empty file and write the
            --  preprocessed sources to Filename.

            Args.Append (+"-x");
            Args.Append (+L);
            if Std'Length /= 0 then
               Args.Append (+Std);
            end if;
            Args.Append (+"-E");
            Args.Append (+"-dM");
            Args.Append (+"-");

            Run_Command
              (Command             => Compiler,
               Arguments           => Args,
               Origin_Command_Name =>
                 "getting built-in macros for " & (+Key),
               Output_File         => Filename,
               In_To_Null          => True);

            --  Decode all macro definitions in Filename and store them in
            --  Result.

            Open (File, In_File, Filename);
            while not End_Of_File (File) loop
               declare
                  Line    : constant String := Get_Line (File);
                  Matches : Match_Array (0 .. 2);
                  M       : Macro_Definition;
               begin
                  Match (Macro_Def_Regexp, Line, Matches);
                  if Matches (0) = No_Match then
                     Warn
                       ("Cannot parse a built-in macro definition for "
                        & Compiler & ", ignoring it:"
                        & ASCII.LF & "  " & Line);
                  else
                     M.Define := True;
                     Append
                       (M.Value, Line (Matches (1).First .. Matches (1).Last));
                     Append (M.Value, "=");
                     Append
                       (M.Value, Line (Matches (2).First .. Matches (2).Last));
                     Result.Append (M);
                  end if;
               end;
            end loop;
            Close (File);
            Delete_File (Filename);

            --  Save Result in the cache for later use

            Compiler_Macros.Insert (Key, Result);
         end;
      end if;

      return Macro_Vector_Cst_Access (Result);
   end Builtin_Macros;

   -----------------------
   -- Preprocess_Source --
   -----------------------

   procedure Preprocess_Source
     (Filename     : String;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      PP_Filename  : out Unbounded_String;
      Info         : in out Project_Info;
      Options      : in out Analysis_Options)
   is
      Cmd : Command_Type;
      --  The command to preprocess the file

      Success : Boolean;
      --  Whether this command is successful

      PID                : constant Unsigned_64 :=
        Unsigned_64 (Pid_To_Integer (Current_Process_Id));
      PP_Output_Filename : constant String :=
        (+Info.Output_Dir) /
        ("pp-output-" & Strip_Zero_Padding (Hex_Image (PID)));
      PP_Output_File     : Ada.Text_IO.File_Type;
   begin
      PP_Filename := +Register_New_File (Info, Filename);

      Cmd :=
        (Command => +Compiler_Driver (Info.Project, Instrumenter.Language),
         others  => <>);

      --  Add the preprocessing flag

      Append_Arg (Cmd, "-E");
      Add_Options (Cmd.Arguments, Options, Pass_Builtins => False);

      Append_Arg (Cmd, Filename);

      --  Register the preprocessing command. We need it to preprocess the file
      --  when producing the report, and getting the text of macro expansions.
      --  We don't need the options added afterwards, as they are just there
      --  for the instrumentation process (and we do not want to pass a -o
      --  option, as it would make paths too opaque at gnatcov coverage time).

      PP_Cmds.Insert
        (Get_Index_From_Generic_Name
           (Filename,
            Kind                => Files_Table.Source_File,
            Indexed_Simple_Name => True),
         Cmd);

      --  To get the include paths, we use the verbose output of cpp -E

      Append_Arg (Cmd, "-v");
      Append_Arg (Cmd, "-o");
      Append_Arg (Cmd, +PP_Filename);

      --  Run the preprocessing command, keep track of whether it was
      --  successful for later

      Success := Run_Command
        (Command             => Cmd,
         Origin_Command_Name => "Preprocessing",
         Output_File         => PP_Output_Filename,
         Ignore_Error        => True);

      --  Clear the search path so that we populate it from the include search
      --  paths in the logs.

      Options.PP_Search_Path.Clear;

      --  Retrieve the include search paths. They are delimited by:
      --  #include "..." search starts here:
      --  #include <...> search starts here:
      --  ...
      --  End of search list

      Open (PP_Output_File, In_File, PP_Output_Filename);

      declare
         RE_Begin_Pattern      : constant Pattern_Matcher :=
           Compile ("#include .* search starts here");
         Begin_Pattern_Matched : Boolean := False;
         RE_End_Pattern        : constant Pattern_Matcher :=
           Compile ("End of search list");
         Matches               : Match_Array (0 .. 0);
      begin
         while not End_Of_File (PP_Output_File) loop
            declare
               Line : constant String := Get_Line (PP_Output_File);
            begin
               Match (RE_Begin_Pattern, Line, Matches);
               if Matches (0) /= No_Match then
                  Begin_Pattern_Matched := True;

               elsif Begin_Pattern_Matched then

                  --  We have already found the "begin" pattern: unless we find
                  --  the "end" pattern (in which case our lookup loop is
                  --  over), the current line contains a search path.

                  Match (RE_End_Pattern, Line, Matches);
                  exit when Matches (0) /= No_Match;
                  Options.PP_Search_Path.Append
                    (Trim (+Line, Ada.Strings.Left));
               end if;
            end;
         end loop;

         --  If the command failed, forward the error message so that users can
         --  investigate what is wrong.

         if not Success then

            --  If the begin pattern for includes was found, the error message
            --  is supposed to appear right after it. If we could not find it,
            --  the error message can be anywhere, so just forward the whole
            --  output.

            if not Begin_Pattern_Matched then
               Reset (PP_Output_File);
            end if;

            while not End_Of_File (PP_Output_File) loop
               Put_Line (Get_Line (PP_Output_File));
            end loop;
            Delete (PP_Output_File);
            Fatal_Error ("Preprocessing failed: aborting");
         end if;
      end;

      Delete (PP_Output_File);
   end Preprocess_Source;

   --------------------------
   -- Common_Parse_TU_Args --
   --------------------------

   function Common_Parse_TU_Args return String_Vectors.Vector is
      Command_Line_Args : String_Vectors.Vector;
      use String_Vectors;
   begin
      --  We will get errors when parsing a gcc-preprocessed file with clang:
      --  portions of the standard library may refer to compiler builtins.
      --  To work around clang incompletely parsing the file in that case,
      --  unset the error limit.

      Append (Command_Line_Args, +"-ferror-limit=0");

      return Command_Line_Args;
   end Common_Parse_TU_Args;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self         : out C_Source_Rewriter;
      Info         : in out Project_Info;
      Filename     : String;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Preprocessed : Boolean := False)
   is
      PP_Filename : Unbounded_String := +Filename;

      Options : Analysis_Options;
      Args    : String_Vectors.Vector;
   begin
      Import_Options (Options, Instrumenter.Language, Info, Filename);
      if not Preprocessed then
         Preprocess_Source
           (Filename, Instrumenter, PP_Filename, Info, Options);
      end if;

      Self.CIdx :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      Add_Options (Args, Options);
      String_Vectors.Append (Args, Common_Parse_TU_Args);
      declare
         C_Args : chars_ptr_array := To_Chars_Ptr_Array (Args);
      begin
         Self.TU :=
           Parse_Translation_Unit
             (C_Idx                 => Self.CIdx,
              Source_Filename       => +PP_Filename,
              Command_Line_Args     => C_Args'Address,
              Num_Command_Line_Args => C_Args'Length,
              Unsaved_Files         => null,
              Num_Unsaved_Files     => 0,
              Options               => 0);
         if Self.TU = null then
            Outputs.Error ("Failed to parse " & Filename);
            Outputs.Error ("Please make sure that the original project can"
                           & " be compiled, and that the right set of"
                           & " options is passed to gnatcov instrument");
            raise Xcov_Exit_Exc;
         end if;
         if Verbose then
            Run_Diagnostics (Self.TU);
         end if;
         Free (C_Args);
      end;
      Self.Rewriter := CX_Rewriter_Create (Self.TU);
      Self.Output_Filename := PP_Filename;
   end Start_Rewriting;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out C_Source_Rewriter) is
      Ignored_Res : Interfaces.C.int;
   begin
      Ignored_Res :=
        CX_Rewriter_Overwrite_Changed_Files (Rew => Self.Rewriter);
   end Apply;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out C_Source_Rewriter) is
   begin
      --  Initialize CIdx to a dummy value so that we can determine in Finalize
      --  whether the Clang data structures were created.

      Self.CIdx := Index_T (System.Null_Address);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_Source_Rewriter) is
   begin
      --  If we have not created the Clang data structures yet, do not try to
      --  free them.

      if System.Address (Self.CIdx) = System.Null_Address then
         return;
      end if;

      Dispose_Translation_Unit (Self.TU);
      Dispose_Index (Self.CIdx);
      if Switches.Pretty_Print then
         Run_Clang_Format (+Self.Output_Filename);
      end if;
   end Finalize;

   ---------------
   -- Emit_SCOs --
   ---------------

   procedure Record_PP_Info
     (Unit_Info : Instrumented_Unit_Info;
      IC        : in out Inst_Context;
      UIC       : in out C_Unit_Inst_Context)
   is
      Orig_Filename : constant String  := +Unit_Info.Filename;
      Args          : String_Vectors.Vector;
   begin
      UIC.Pass := Record_PP_Info_Pass'Access;
      UIC.CIdx :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      --  Get the predefined macros and search paths of the user's compiler and
      --  inhibit the use of clang predefined macros. We want to fully emulate
      --  the user's preprocessor.

      Add_Options (Args, UIC.Options);
      String_Vectors.Append (Args, Common_Parse_TU_Args);

      --  TODO??? We should also inhibit the use of clang predefined macros,
      --  with the -undef option, but doing this yields parsing errors, and a
      --  structurally different AST, and a mismatching between the
      --  preprocessing phase, and the instrumentation phase. For whatever
      --  reason, this is not the case when we undefine manually (passing -U
      --  to the command line) all of clang predefines... But we can't do that
      --  as getting the clang predefines requires having it on the path (and
      --  it is not packaged with gnatcov as of right now, and we probably
      --  don't want to do that).
      --
      --  As this is best effort, remove the -undef switch for now, and
      --  investigate later (it looks complex). This means that if the code
      --  base has compiler-specific code e.g. code that is expanded when
      --  preprocessing with clang predefines, but not when compiling with
      --  gcc, and admitting the compiler driver is gcc we will get a
      --  different AST in the preprocessing phase, and simply discard the
      --  preprocessing information later.

      declare
         C_Args : chars_ptr_array := To_Chars_Ptr_Array (Args);
      begin
         UIC.TU :=
           Parse_Translation_Unit
             (C_Idx                 => UIC.CIdx,
              Source_Filename       => Orig_Filename,
              Command_Line_Args     => C_Args'Address,
              Num_Command_Line_Args => C_Args'Length,
              Unsaved_Files         => null,
              Num_Unsaved_Files     => 0,
              Options               => Translation_Unit_Keep_Going);
         Free (C_Args);
      end;
      Traverse_Declarations
        (IC  => IC,
         UIC => UIC,
         L   => Get_Children (Get_Translation_Unit_Cursor (UIC.TU)));

   end Record_PP_Info;

   ----------------------------
   -- Instrument_Source_File --
   ----------------------------

   procedure Instrument_Source_File
     (CU_Name      : Compilation_Unit_Name;
      Unit_Info    : Instrumented_Unit_Info;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj_Info     : in out Project_Info;
      IC           : in out Inst_Context;
      UIC          : out C_Unit_Inst_Context)
   is
      Orig_Filename : constant String  := +Unit_Info.Filename;
      PP_Filename   : Unbounded_String;
      --  Respectively original, and preprocessed filename

      Buffer_Filename : constant String :=
        To_Symbol_Name (Sys_Buffers) & "_b_" & Instrumented_Unit_Slug (CU_Name)
        & Source_Suffix (Instrumenter, GPR.Unit_Body, Prj_Info.Project);
      --  Name of the generated source file holding the coverage buffers

      Rewriter : C_Source_Rewriter;
      --  Holds the compilation index, the translation unit and the rewriter.
      --  We will use shortcuts to the translation unit and the rewriter in the
      --  C instrumentation context.

      Record_PP_Info_Last_SCO : Nat;
      --  Index of the last SCO in SCOs.SCO_Table after the first pass. Used
      --  to check against the result of the second pass (when we instrument
      --  and fill again the SCOs.SCO_Table), to be sure that both passes
      --  count the same number of SCOs. For more details, see the
      --  documentation of Pass_Kind in instrument-c.ads.

      Insert_Extern_Location : Source_Location_T;
      --  Where to insert extern declarations

      procedure Put_Extern_Decl
        (C_Type     : String;
         Name       : String;
         Array_Size : String := "";
         Func_Args  : String := "");
      --  Local shortcut to avoid passing UIC.TU/UIC.Rewriter explicitly

      ---------------------
      -- Put_Extern_Decl --
      ---------------------

      procedure Put_Extern_Decl
        (C_Type     : String;
         Name       : String;
         Array_Size : String := "";
         Func_Args  : String := "") is
      begin
         Put_Extern_Decl
           (UIC.Rewriter,
            Insert_Extern_Location,
            Instrumenter,
            C_Type,
            Name,
            Array_Size,
            Func_Args);
      end Put_Extern_Decl;

   --  Start of processing for Instrument_Source_File

   begin
      SCOs.Initialize;
      UIC.SFI := Get_Index_From_Generic_Name
        (Orig_Filename,
         Kind                => Files_Table.Source_File,
         Indexed_Simple_Name => True);

      --  Initialize the C instrumentation context

      UIC.Instrumented_Unit := CU_Name;
      UIC.Buffer_Unit :=
        CU_Name_For_File (+Buffer_Filename, CU_Name.Project_Name);
      UIC.File := +Orig_Filename;

      --  Create the only SCO Unit we will need to instrument this source file.
      --  Also make sure we will create at least the set of coverage buffers
      --  for the main source file.

      declare
         Dummy : C_Instrumented_Entities renames
           UIC.Find_Instrumented_Entities (UIC.SFI);
      begin
         SCOs.SCO_Unit_Table.Append
           ((File_Name  => new String'(Orig_Filename),
             File_Index => UIC.SFI,
             Dep_Num    => 1,
             From       => SCOs.SCO_Table.First,
             To         => SCOs.SCO_Table.Last));
      end;

      --  Import analysis options for the file to preprocess, then run the
      --  preprocessor.

      Import_Options
        (UIC.Options, Instrumenter.Language, Prj_Info, Orig_Filename);
      Preprocess_Source
        (Orig_Filename, Instrumenter, PP_Filename, Prj_Info, UIC.Options);

      --  Start by recording preprocessing information

      Record_PP_Info (Unit_Info, IC, UIC);

      --  Then record exemption annotations in the comments. Reuse the TU from
      --  the preprocessing information recording pass as we want the
      --  annotations to refer to unpreprocessed locations.

      Search_Exemptions :
      declare
         Start_Matcher : constant Pattern_Matcher :=
           Compile ("GNATCOV_EXEMPT_ON ""(.*)""");
         End_Matcher   : constant Pattern_Matcher :=
           Compile ("GNATCOV_EXEMPT_OF");
         Match_Res     : Match_Array (0 .. 1);

         procedure Search_Exempt_In_Token (Token : Token_T);

         ----------------------------
         -- Search_Exempt_In_Token --
         ----------------------------

         procedure Search_Exempt_In_Token (Token : Token_T) is
         begin
            if Get_Token_Kind (Token) = Token_Comment then
               declare
                  Comment : constant String :=
                    Get_Token_Spelling (UIC.TU, Token);
                  Ann     : ALI_Annotation;
               begin
                  Match (Start_Matcher, Comment, Match_Res);
                  if Match_Res (1) /= No_Match then
                     Ann.Kind := Exempt_On;
                     Ann.Message :=
                       new String'
                         (Comment (Match_Res (1).First .. Match_Res (1).Last));
                     UIC.Annotations.Append
                       (((UIC.SFI, Sloc (Get_Token_Location (UIC.TU, Token))),
                         Ann));
                  end if;
                  Match (End_Matcher, Comment, Match_Res);
                  if Match_Res (0) /= No_Match then
                     Ann.Kind := Exempt_Off;
                     UIC.Annotations.Append
                       (((UIC.SFI,
                          Sloc
                           (Get_Range_End (Get_Token_Extent (UIC.TU, Token)))),
                         Ann));
                  end if;
               end;
            end if;
         end Search_Exempt_In_Token;
      begin
         Iterate_Tokens
           (UIC.TU,
            Get_Translation_Unit_Cursor (UIC.TU),
            Search_Exempt_In_Token'Access);
      end Search_Exemptions;

      --  Flush the SCO_Table. We want the SCOs to be located at their
      --  presumed (i.e. accounting for line directives) preprocessed source
      --  range, to have unique locations for each.

      Record_PP_Info_Last_SCO := SCOs.SCO_Table.Last;
      SCOs.SCO_Table.Init;

      --  Then, instrument

      UIC.Pass := Instrument_Pass'Access;

      Start_Rewriting (Self         => Rewriter,
                       Info         => Prj_Info,
                       Filename     => +PP_Filename,
                       Instrumenter => Instrumenter,
                       Preprocessed => True);
      UIC.TU := Rewriter.TU;
      UIC.Rewriter := Rewriter.Rewriter;
      Insert_Extern_Location := Find_First_Insert_Location (UIC.TU);

      Traverse_Declarations
        (IC  => IC,
         UIC => UIC,
         L   => Get_Children (Get_Translation_Unit_Cursor (UIC.TU)));

      --  Check whether there is a mismatch between Last_SCO and
      --  SCOs.SCO_Table.Last. If there is, warn the user and discard the
      --  preprocessed info: the SCO mapping will be wrong.
      --  Note that this will result on possibly wrong column numbering in
      --  the report (as a macro expansion may offset a block of code by
      --  an arbitrary amount).

      if Record_PP_Info_Last_SCO /= SCOs.SCO_Table.Last then
         Outputs.Warn
           (Orig_Filename & ": preprocessed file coverage obligations"
            &  " inconsistent with original file obligations (expecting"
            & Nat'Image (Record_PP_Info_Last_SCO) & " coverage obligations,"
            & " but got" & Nat'Image (SCOs.SCO_Table.Last)
            & ". Discarding preprocessing information.");
         UIC.LL_PP_Info_Map.Clear;
      end if;

      --  Now that the set of coverage obligations and the set of source files
      --  are known, allocate the required set of coverage buffers (one per
      --  source file) and assign the corresponding buffers indexes.

      for Cur in UIC.Instrumented_Entities.Iterate loop
         UIC.Instrumented_Entities.Reference (Cur).Buffers_Index :=
           Create_Unit_Bits
             (UIC.Allocated_Bits, C_Instrumented_Entities_Maps.Key (Cur));
      end loop;

      --  Convert low level SCOs from the instrumenter to high level SCOs.
      --  This creates BDDs for every decision.

      declare
         SCO_Map : aliased LL_HL_SCO_Map :=
           (SCOs.SCO_Table.First .. SCOs.SCO_Table.Last => No_SCO_Id);
      begin
         Process_Low_Level_SCOs
           (Provider      => SC_Obligations.Instrumenter,
            Origin        => UIC.SFI,
            Created_Units => UIC.CUs,
            SCO_Map       => SCO_Map'Access,
            Count_Paths   => True);

         --  Now that we have computed high level SCOs, remap the current
         --  SCO macro info map in our unit instrumentation context (bound to
         --  low level SCOs), and add it to the global macro info map.

         for Cursor in UIC.LL_PP_Info_Map.Iterate loop
            declare
               use LL_SCO_PP_Info_Maps;
               LL_SCO : constant Nat := Key (Cursor);
               Info   : constant PP_Info := Element (Cursor);
            begin
               Add_PP_Info (SCO_Map (LL_SCO), Info);
            end;
         end loop;

         --  Import annotations in our internal tables

         UIC.Import_Annotations (UIC.CUs);
         Filter_Annotations;

         for Cur in UIC.Instrumented_Entities.Iterate loop
            declare
               SFI : constant Valid_Source_File_Index :=
                 C_Instrumented_Entities_Maps.Key (Cur);
               Ent : C_Instrumented_Entities renames
                 UIC.Instrumented_Entities.Reference (Cur);

               Unit_Bits : Allocated_Bits renames
                 UIC.Allocated_Bits.Reference (Ent.Buffers_Index);
               Bit_Maps  : CU_Bit_Maps;
            begin
               --  Allocate bits in covearge buffers and insert the
               --  corresponding witness calls.

               for SS of Ent.Statements loop
                  Insert_Statement_Witness (UIC, Ent.Buffers_Index, SS);
               end loop;

               if Coverage.Enabled (Coverage_Options.Decision)
                  or else MCDC_Coverage_Enabled
               then
                  for SD of Ent.Decisions loop
                     Insert_Decision_Witness
                       (UIC,
                        Ent.Buffers_Index,
                        SD,
                        Path_Count (SCO_Map (SD.LL_SCO)));
                  end loop;

                  if MCDC_Coverage_Enabled then

                     --  As high-level SCO tables have been populated, we have
                     --  built BDDs for each decisions, and we can now set the
                     --  correct MC/DC path offset for each condition.

                     for SC of Ent.Conditions loop
                        declare
                           Condition : constant SCO_Id := SCO_Map (SC.LL_SCO);
                           Decision  : constant SCO_Id :=
                             Enclosing_Decision (Condition);
                        begin
                           if Path_Count (Decision) = 0 then
                              Set_Decision_SCO_Non_Instr_For_MCDC (Decision);
                           else
                              Insert_Condition_Witness
                                (UIC, SC, Offset_For_True (Condition));
                           end if;
                        end;
                     end loop;
                  end if;
               end if;

               --  Create mappings from alloacted bits to the corresponding SCO
               --  discharging information (Bit_Maps).

               Bit_Maps :=
                 (Statement_Bits => new Statement_Bit_Map'
                    (Bit_Id'First .. Unit_Bits.Last_Statement_Bit =>
                       No_SCO_Id),
                  Decision_Bits  => new Decision_Bit_Map'
                    (Bit_Id'First .. Unit_Bits.Last_Outcome_Bit =>
                       (No_SCO_Id, False)),
                  MCDC_Bits      => new MCDC_Bit_Map'
                    (Bit_Id'First .. Unit_Bits.Last_Path_Bit =>
                       (No_SCO_Id, 0)));

               for S_Bit_Alloc of Unit_Bits.Statement_Bits loop
                  Bit_Maps.Statement_Bits (S_Bit_Alloc.Executed) :=
                    SCO_Map (S_Bit_Alloc.LL_S_SCO);
               end loop;

               for D_Bit_Alloc of Unit_Bits.Decision_Bits loop
                  declare
                     D_SCO : constant SCO_Id := SCO_Map (D_Bit_Alloc.LL_D_SCO);
                  begin
                     for Outcome in Boolean loop
                        Bit_Maps.Decision_Bits
                          (D_Bit_Alloc.Outcome_Bits (Outcome)) :=
                          (D_SCO, Outcome);
                     end loop;

                     if MCDC_Coverage_Enabled
                       and then D_Bit_Alloc.Path_Bits_Base /= No_Bit_Id
                     then
                        declare
                           Path_Count : constant Natural :=
                             SC_Obligations.Path_Count (D_SCO);
                        begin
                           for J in 1 .. Any_Bit_Id (Path_Count) loop
                              Bit_Maps.MCDC_Bits
                                (D_Bit_Alloc.Path_Bits_Base + J - 1) :=
                                (D_SCO, Natural (J - 1));
                           end loop;
                        end;
                     end if;
                  end;
               end loop;

               --  Associate these bit maps to the corresponding CU

               Set_Bit_Maps (UIC.CUs.Element (SFI), Bit_Maps);
            end;
         end loop;
      end;

      --  We import the extern declaration of symbols instead of including the
      --  header where they are defined.
      --
      --  This would be easier, but we then run into issues when including
      --  unpreprocessed files (and the source files generated with
      --  instrumentation and the sources from GNATcov_RTS are not
      --  preprocessed). Indeed, these unpreprocessed files are preprocessed at
      --  build time, and they may include standard libraries, which were
      --  already preprocessed at instrumentation time.
      --
      --  This may result in doubling declarations coming from these standard
      --  libraries, which we don't want. To be safe, we will declare
      --  GNATcov_RTS functions and symbols as extern.

      declare
         Buffers_Count : constant String :=
           Img (Natural (UIC.Allocated_Bits.Length));
      begin
         Put_Extern_Decl
           ("unsigned char *",
            Statement_Buffer_Symbol (UIC.Instrumented_Unit),
            Array_Size => Buffers_Count);
         Put_Extern_Decl
           ("unsigned char *",
            Decision_Buffer_Symbol (UIC.Instrumented_Unit),
            Array_Size => Buffers_Count);
         Put_Extern_Decl
           ("unsigned char *",
            MCDC_Buffer_Symbol (UIC.Instrumented_Unit),
            Array_Size => Buffers_Count);
      end;
      Put_Extern_Decl
        ("unsigned",
         "gnatcov_rts_witness",
         Func_Args => "unsigned char *buffer_address, unsigned bit_id");
      Put_Extern_Decl
        ("unsigned",
         "gnatcov_rts_witness_decision",
         Func_Args => "unsigned char *buffer_address,"
                      & " unsigned false_bit,"
                      & " unsigned true_bit,"
                      & " unsigned value");
      Put_Extern_Decl
        ("unsigned",
         "gnatcov_rts_witness_decision_mcdc",
         Func_Args => "unsigned char *decision_buffer_address,"
                      & " unsigned false_bit,"
                      & " unsigned true_bit,"
                      & " unsigned char *mcdc_buffer_address,"
                      & " unsigned mcdc_base,"
                      & " unsigned *mcdc_path_address,"
                      & " unsigned value");
      Put_Extern_Decl
        ("unsigned",
         "gnatcov_rts_witness_condition",
         Func_Args => "unsigned *mcdc_path_address,"
                      & "unsigned offset_for_true,"
                      & " unsigned first,"
                      & " unsigned value");

      --  Insert automatic buffer dump calls, if requested

      if IC.Dump_Config.Trigger /= Manual and then Unit_Info.Is_Main then
         Auto_Dump_Buffers_In_Main
           (IC           => IC,
            Info         => Prj_Info,
            Main         => UIC.Instrumented_Unit,
            Rew          => Rewriter,
            Instrumenter => Instrumenter);
      end if;
      Rewriter.Apply;
   end Instrument_Source_File;

   ----------------------------
   -- Format_Array_Init_Expr --
   ----------------------------

   function Format_Array_Init_Expr
     (Exprs     : String_Vectors.Vector;
      Multiline : Boolean := False) return String
   is
      Result : Unbounded_String;
   begin
      Append (Result, "{");
      if Multiline then
         Append (Result, ASCII.LF);
      end if;
      for I in Exprs.First_Index .. Exprs.Last_Index loop
         if I > Exprs.First_Index then
            if Multiline then
               Append (Result, "," & ASCII.LF);
            else
               Append (Result, ", ");
            end if;
         end if;
         if Multiline then
            Append (Result, "  ");
         end if;
         Append (Result, To_String (Exprs.Element (I)));
      end loop;
      if Multiline then
         Append (Result, ASCII.LF);
      end if;
      Append (Result, "}");
      return To_String (Result);
   end Format_Array_Init_Expr;

   ------------------------
   -- Format_Fingerprint --
   ------------------------

   function Format_Fingerprint
     (Fingerprint : SC_Obligations.SCOs_Hash) return String is
      Items : String_Vectors.Vector;
   begin
      for Byte of Fingerprint loop
         Items.Append (+Img (Integer (Byte)));
      end loop;

      return Format_Array_Init_Expr (Items);
   end Format_Fingerprint;

   ----------------------
   -- Emit_Buffer_Unit --
   ----------------------

   procedure Emit_Buffer_Unit
     (Info         : in out Project_Info;
      UIC          : C_Unit_Inst_Context'Class;
      Instrumenter : C_Family_Instrumenter_Type'Class)
   is
      CU_Name : Compilation_Unit_Name renames UIC.Buffer_Unit;
      File    : Text_Files.File_Type;

      Buffers_Count : constant Natural := Natural (UIC.Allocated_Bits.Length);
      --  Number of items in the buffer group to create

      --  Symbol names for the arrays of buffer bits (one for statements, one
      --  for decisions, one for MC/DC paths). They are exported, so they need
      --  to be unique program-wide.

      Statement_Buffers : constant String :=
        Statement_Buffer_Symbol (UIC.Instrumented_Unit);
      Decision_Buffers  : constant String :=
        Decision_Buffer_Symbol (UIC.Instrumented_Unit);
      MCDC_Buffers      : constant String :=
        MCDC_Buffer_Symbol (UIC.Instrumented_Unit);

      Buffers_Array : constant String := "__xcov_buffers_group";
      --  Name of the array of gnatcov_rts_coverage_buffers struct for this
      --  unit.

      Statement_Init : String_Vectors.Vector;
      Decision_Init  : String_Vectors.Vector;
      MCDC_Init      : String_Vectors.Vector;
      --  Initialization expressions for the pointer arrays: each element in a
      --  vector is an array item initializer.

      Group_Init : String_Vectors.Vector;
      --  Likewise, but for the coverage buffers group

      Buffers_CUs      : CU_Id_Vectors.Vector :=
        CU_Id_Vectors.To_Vector (No_CU_Id, UIC.Allocated_Bits.Length);
      Buffers_CU_Names : CU_Name_Vectors.Vector :=
        CU_Name_Vectors.To_Vector
           ((Language_Kind => File_Based_Language, others => <>),
            UIC.Allocated_Bits.Length);
      --  For each set of buffers in UIC.Allocated_Bits, corresponding CU_Id
      --  and CU_Name for the instrumented source file.

      I : Positive := 1;
   begin
      --  Compute Buffers_Filenames and Buffers_Projects

      for Cur in UIC.Sources_Of_Interest.Iterate loop
         declare
            use Created_Unit_Maps;

            SOI   : Source_Of_Interest renames
              UIC.Sources_Of_Interest.Constant_Reference (Cur);
            Cur   : Cursor;
            Index : Natural;
         begin
            if SOI.Of_Interest then
               Cur := UIC.CUs.Find (SOI.SFI);
               if Has_Element (Cur) then
                  Index :=
                    UIC.Instrumented_Entities.Constant_Reference
                      (SOI.SFI).Buffers_Index;
                  Buffers_CUs (Index) := Element (Cur);
                  Buffers_CU_Names (Index) := SOI.CU_Name;
               end if;
            end if;
         end;
      end loop;

      --  Each source file in UIC.Instrumented_Entities is supposed to have a
      --  corresponding "Of_Interest => True" entry in UIC.Sources_Of_Interest.

      pragma Assert (for all CU of Buffers_CUs => CU /= No_CU_Id);

      --  Start to emit the buffer unit

      Create_File (Info, File, +CU_Name.Filename);

      File.Put_Line ("#include ""gnatcov_rts_c-buffers.h""");
      File.New_Line;

      --  Define coverage buffers for each source file:
      --
      --  * First, create individual buffers for each kind (statement, decision
      --    and MC/DC). These are static, so no need to create symbol names
      --    that are unique program-wide.
      --
      --  * Then, create pointers to these individual buffers. Put all pointers
      --    to each kind of buffers in an exported array. Since both pointers
      --    and the exported array are exported (used in buffer list units and
      --    instrumented units), we need to create symbol names that are unique
      --    program-wide.

      --  Create the static buffers as well as the
      --  gnatcov_rts_coverage_buffers struct for each source file.

      for Cur in UIC.Instrumented_Entities.Iterate loop
         declare
            SFI           : constant Valid_Source_File_Index :=
              C_Instrumented_Entities_Maps.Key (Cur);
            Buffers_Index : constant Positive :=
              UIC.Instrumented_Entities.Constant_Reference (SFI).Buffers_Index;
            Unit_Bits     : Allocated_Bits renames
              UIC.Allocated_Bits.Constant_Reference (Buffers_Index);

            CU      : constant CU_Id := Buffers_CUs (Buffers_Index);
            CU_Name : Compilation_Unit_Name renames
              Buffers_CU_Names.Constant_Reference (Buffers_Index);

            --  Symbol name for each kind of static buffer

            Suffix : constant String := "_" & Img (I);

            Statement_Buffer_Repr : constant String := "__stmt_bits" & Suffix;
            Decision_Buffer_Repr  : constant String := "__dc_bits" & Suffix;
            MCDC_Buffer_Repr      : constant String := "__mcdc_bits" & Suffix;

            Buffers_Struct : constant String := "__xcov_buffers" & Suffix;
            --  Name of the gnatcov_rts_coverage_buffers struct for this
            --  source file.

         begin
            File.Put_Line ("/* Buffers for " & Get_Full_Name (SFI) & " */");

            --  Static buffers

            File.Put_Line
              ("static unsigned char " & Statement_Buffer_Repr & "["
               & Img (Any_Bit_Id'Max (1, Unit_Bits.Last_Statement_Bit + 1))
               & "];");
            Statement_Init.Append (+("&" & Statement_Buffer_Repr & "[0]"));

            File.Put_Line
              ("static unsigned char " & Decision_Buffer_Repr & "["
               & Img (Any_Bit_Id'Max (1, Unit_Bits.Last_Outcome_Bit + 1))
               & "];");
            Decision_Init.Append (+("&" & Decision_Buffer_Repr & "[0]"));

            File.Put_Line
              ("static unsigned char " & MCDC_Buffer_Repr & "["
               & Img (Any_Bit_Id'Max (1, Unit_Bits.Last_Path_Bit + 1))
               & "];");
            MCDC_Init.Append (+("&" & MCDC_Buffer_Repr & "[0]"));

            --  gnatcov_rts_coverage_buffers struct

            File.Put_Line
              ("static const struct gnatcov_rts_coverage_buffers "
               & Buffers_Struct & " = {"
               & ASCII.LF
               & "  .fingerprint = "
               & Format_Fingerprint (SC_Obligations.Fingerprint (CU)) & ","
               & ASCII.LF
               & "  .language_kind = FILE_BASED_LANGUAGE,"
               & ASCII.LF
               & "  .unit_part = NOT_APPLICABLE_PART,"
               & ASCII.LF

               --  Old toolchains (for instance GNAT Pro 7.1.2) consider that
               --  "STR(<string literal>)" is not a static expression, and thus
               --  refuse using STR to initialize a global data structure. To
               --  workaround this, emit a gnatcov_rtr_string literal
               --  ourselves.

               & "  .unit_name = " & Format_Str_Constant (+CU_Name.Filename)
               & ","
               & ASCII.LF
               & "  .project_name = "
               & Format_Str_Constant (+CU_Name.Project_Name) & ","
               & ASCII.LF

               --  We do not use the created pointer (Statement_Buffer) to
               --  initialize the buffer fields, as this is rejected by old
               --  versions of the compiler (up to the 20 version): the
               --  initializer element is considered not constant. To work
               --  around it, we simply use the original expression instead of
               --  using a wrapper pointer.

               & "  .statement = &" & Statement_Buffer_Repr & "[0],"
               & ASCII.LF
               & "  .decision = &" & Decision_Buffer_Repr & "[0],"
               & ASCII.LF
               & "  .mcdc = &" & MCDC_Buffer_Repr & "[0],"
               & ASCII.LF

               & "  .statement_last_bit = "
               & Img (Unit_Bits.Last_Statement_Bit) & ","
               & ASCII.LF
               & "  .decision_last_bit = " & Img (Unit_Bits.Last_Outcome_Bit)
               & ","
               & ASCII.LF
               & "  .mcdc_last_bit = " & Img (Unit_Bits.Last_Path_Bit)
               & ASCII.LF
               & "};");
            Group_Init.Append (+("&" & Buffers_Struct));

            File.New_Line;

            --  Track which CU_Id maps to which instrumented unit

            Instrumented_Unit_CUs.Insert (CU_Name, CU);

            I := I + 1;
         end;
      end loop;

      declare
         Buffers_Count_Img : constant String := Img (Buffers_Count);
      begin
         --  Then define pointers to these individual buffers. Put all pointers
         --  to statement buffers in an exported array (same for decision and
         --  for MC/DC).

         Put_Format_Def
           (File,
            Instrumenter,
            "unsigned char *const",
            Statement_Buffers,
            Array_Size => Buffers_Count_Img,
            Init_Expr  => Format_Array_Init_Expr
                            (Statement_Init, Multiline => True));
         Put_Format_Def
           (File,
            Instrumenter,
            "unsigned char *const",
            Decision_Buffers,
            Array_Size => Buffers_Count_Img,
            Init_Expr  => Format_Array_Init_Expr
                            (Decision_Init, Multiline => True));
         Put_Format_Def
           (File,
            Instrumenter,
            "unsigned char *const",
            MCDC_Buffers,
            Array_Size => Buffers_Count_Img,
            Init_Expr  => Format_Array_Init_Expr
                            (MCDC_Init, Multiline => True));

         --  Create the buffers group: first the array of buffers (static),
         --  then the gnatcov_rts_coverage_buffers_group struct (extern).

         File.Put_Line
           ("static const struct gnatcov_rts_coverage_buffers *"
            & Buffers_Array & "[" & Buffers_Count_Img & "] = "
            & Format_Array_Init_Expr (Group_Init, Multiline => True) & ";");
         Put_Format_Def
           (File,
            Instrumenter,
            "const struct gnatcov_rts_coverage_buffers_group",
            Unit_Buffers_Name (UIC.Instrumented_Unit),
            Init_Expr =>
              "{" & Img (Buffers_Count) & ", &" & Buffers_Array & "[0]}");
      end;
   end Emit_Buffer_Unit;

   ---------------------------
   -- Emit_Dump_Helper_Unit --
   ---------------------------

   procedure Emit_Dump_Helper_Unit
     (IC           : Inst_Context;
      Info         : in out Project_Info;
      Main         : Compilation_Unit_Name;
      Helper_Unit  : out US.Unbounded_String;
      Instrumenter : C_Family_Instrumenter_Type'Class)
   is
      File : Text_Files.File_Type;

      Output_Proc : constant String :=
        (case IC.Dump_Config.Channel is
            when Binary_File => "gnatcov_rts_write_trace_file_wrapper",
            when Base64_Standard_Output =>
              "gnatcov_rts_write_trace_file_base64");

      Indent1 : constant String := "    ";
      Indent2 : constant String := Indent1 & "  ";

   begin
      --  Create the name of the helper unit

      Helper_Unit :=
        To_Symbol_Name (Sys_Buffers)
        & "_d_"
        & Instrumented_Unit_Slug (Main)
        & US.To_Unbounded_String
            (Source_Suffix (Instrumenter, GPR.Unit_Body, Info.Project));

      --  Compute the qualified names we need for instrumentation

      declare
         Filename       : constant String := +Helper_Unit;
         Dump_Procedure : constant String := Dump_Procedure_Symbol (Main);

      begin
         --  Emit the package body

         Create_File (Info, File, Filename);

         File.Put_Line ("#include ""gnatcov_rts_c_strings.h""");

         case IC.Dump_Config.Channel is
            when Binary_File =>
               File.Put_Line ("#include """
                              & "gnatcov_rts_c-traces-output-files.h""");
               File.Put_Line ("#include ""gnatcov_rts_c-os_interface.h""");
            when Base64_Standard_Output =>
               File.Put_Line ("#include """
                              & "gnatcov_rts_c-traces-output-base64.h""");
         end case;
         File.Put_Line ("#include <stdlib.h>");
         File.Put_Line
           ("#include """ & Buffers_List_Filename (IC)
            & Source_Suffix (Instrumenter, GPR.Unit_Spec, Info.Project)
            & """");

         --  Emit the procedure to write the trace file

         File.New_Line;
         File.Put (Instrumenter.Extern_Prefix);
         File.Put_Line ("void " & Dump_Procedure & " (void) {");

         File.Put_Line (Indent1 & Output_Proc & " (");
         File.Put_Line (Indent2 & "&" & Unit_Buffers_Array_Name (IC) & ",");
         case IC.Dump_Config.Channel is
         when Binary_File =>
            declare
               use GNATCOLL.VFS;

               Env_Var : constant String :=
                 (if US.Length (IC.Dump_Config.Filename_Env_Var) = 0
                  then "GNATCOV_RTS_DEFAULT_TRACE_FILENAME_ENV_VAR"
                  else """" & (+IC.Dump_Config.Filename_Env_Var) & """");
               Prefix  : constant String :=
                 (if US.Length (IC.Dump_Config.Filename_Prefix) = 0
                  then """" & String'(+Info.Project.Executable_Name
                                (+(+Main.Filename),
                                 Include_Suffix => True)) & """"
                  else """" & (+IC.Dump_Config.Filename_Prefix) & """");
               Tag     : constant String := """" & (+IC.Tag) & """";
               Simple  : constant String :=
                 (if IC.Dump_Config.Filename_Simple then "1" else "0");
            begin
               File.Put_Line
                 (Indent2 & "gnatcov_rts_default_trace_filename(");
               File.Put_Line (Indent2 & Env_Var & ",");
               File.Put_Line (Indent2 & Prefix & ",");
               File.Put_Line (Indent2 & Tag & ",");
               File.Put_Line (Indent2 & Simple & "),");

               File.Put_Line (Indent2 & "STR (""" & (+Main.Filename)
                              & """),");
               File.Put_Line (Indent2 & "gnatcov_rts_time_to_uint64()" & ",");
               File.Put_Line (Indent2 & "STR ("""")");
            end;

         when Base64_Standard_Output =>

            --  Configurations using this channel generally run on embedded
            --  targets and have a small runtime, so our best guess for the
            --  program name is the name of the main, and there is no way to
            --  get the current execution time.

            File.Put_Line (Indent2 & "STR (""" & (+Main.Filename) & """),");
            File.Put_Line (Indent2 & "0,");
            File.Put_Line (Indent2 & "STR ("""")");

         end case;
         File.Put_Line (Indent1 & ");");

         File.Put_Line ("}");

         File.Close;
      end;
   end Emit_Dump_Helper_Unit;

   -------------------------------
   -- Auto_Dump_Buffers_In_Main --
   -------------------------------

   procedure Auto_Dump_Buffers_In_Main
     (IC           : Inst_Context;
      Info         : in out Project_Info;
      Main         : Compilation_Unit_Name;
      Rew          : in out C_Source_Rewriter;
      Instrumenter : C_Family_Instrumenter_Type'Class)
   is
      Instr_Units : constant CU_Name_Vectors.Vector :=
        Instr_Units_For_Closure (IC, Main);
      --  List of names for instrumented units

      Helper_Filename : US.Unbounded_String;
      --  Name of file to contain helpers implementing the buffers dump

      Insert_Extern_Location : constant Source_Location_T :=
        Find_First_Insert_Location (Rew.TU);
      --  Where to insert extern declarations

      Main_Cursor : constant Cursor_T := Get_Main (Rew.TU);
      --  Cursor of the main declaration
   begin
      if Instr_Units.Is_Empty then
         return;
      end if;

      if Main_Cursor = Get_Null_Cursor then
         Outputs.Fatal_Error ("Could not find main function in "
                              & (+Main.Filename));
      end if;

      Emit_Dump_Helper_Unit (IC, Info, Main, Helper_Filename, Instrumenter);

      Put_Extern_Decl
        (Rew.Rewriter,
         Insert_Extern_Location,
         Instrumenter,
         "void",
         Dump_Procedure_Symbol (Main),
         Func_Args => "void");

      if IC.Dump_Config.Trigger = Ravenscar_Task_Termination then
         Warn ("--dump-trigger=ravenscar-task-termination is not valid for a C"
               & " main. Defaulting to --dump-trigger=main-end for this"
               & " main.");
      end if;

      case IC.Dump_Config.Trigger is
         when Main_End | Ravenscar_Task_Termination =>
            declare
               function Process
                 (Cursor : Cursor_T) return Child_Visit_Result_T;
               --  Callback for Visit_Children. Insert calls to dump buffers
               --  before the function return.

               -------------
               -- Process --
               -------------

               function Process
                 (Cursor : Cursor_T) return Child_Visit_Result_T is
               begin
                  if Is_Statement (Kind (Cursor))
                    and then Kind (Cursor) = Cursor_Return_Stmt
                  then
                     declare
                        Return_Expr : constant Cursor_T :=
                          Get_Children (Cursor).First_Element;
                     begin
                        --  Introduce a variable to hold the return result,
                        --  and replace "return <expr>;" with
                        --  "{
                        --     int <tmp>;
                        --     return <tmp>=<expr>, <dump_buffers>(), <tmp>;
                        --   }".
                        --
                        --  Note that we have to be careful to put braces
                        --  around the return statement, as the control flow
                        --  structures may not have been curlified yet,
                        --  e.g. if the main is not a unit of interest. This
                        --  means that:
                        --
                        --  if (<cond>)
                        --     return <expr>;
                        --
                        --  should produce
                        --
                        --  if (<cond>)
                        --  {
                        --     int <tmp>;
                        --     return <tmp>=<expr>, <dump_buffers>(), <tmp>;
                        --  }
                        --
                        --  Notice how this would be wrong without the braces.

                        Insert_Text_After_Start_Of
                          (N    => Cursor,
                           Text => "{int gnatcov_rts_return;",
                           Rew  => Rew.Rewriter);

                        Insert_Text_Before_Start_Of
                          (N    => Return_Expr,
                           Text => "gnatcov_rts_return = ",
                           Rew  => Rew.Rewriter);

                        Insert_Text_After_End_Of
                          (N    => Return_Expr,
                           Text => ", "
                                   & Dump_Procedure_Symbol (Main)
                                   & "(), gnatcov_rts_return",
                           Rew  => Rew.Rewriter);

                        CX_Rewriter_Insert_Text_After_Token
                          (Rew.Rewriter,
                           Get_Range_End (Get_Cursor_Extent (Cursor)),
                           "}");

                        return Child_Visit_Continue;
                     end;
                  else
                     --  Be careful not to recurse into lambda expressions,
                     --  which may have their own return statements.

                     return (if Kind (Cursor) = Cursor_Lambda_Expr
                             then Child_Visit_Continue
                             else Child_Visit_Recurse);
                  end if;
               end Process;

            begin
               Visit_Children (Parent  => Main_Cursor,
                               Visitor => Process'Access);
            end;

         when At_Exit =>
            Put_Extern_Decl
              (Rew.Rewriter,
               Insert_Extern_Location,
               Instrumenter,
               "int",
               "atexit",
               Func_Args => "void (*function) (void)");

            declare
               Body_Cursor : constant Cursor_T := Get_Body (Main_Cursor);

               --  The body of a function is a compound statement, so insert
               --  the call to atexit before its first statement.

               Body_Stmts : constant Cursor_Vectors.Vector :=
                 Get_Children (Body_Cursor);
               First_Stmt : constant Cursor_T := Body_Stmts.First_Element;

               Location : constant Source_Location_T :=
                 Get_Cursor_Location (First_Stmt);
            begin
               CX_Rewriter_Insert_Text_Before
                 (Rew    => Rew.Rewriter,
                  Loc    => Location,
                  Insert => "atexit (" & Dump_Procedure_Symbol (Main) & ");");
            end;

         when others =>
            null;
      end case;
   end Auto_Dump_Buffers_In_Main;

   overriding procedure Auto_Dump_Buffers_In_Main
     (Self     : C_Family_Instrumenter_Type;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info)
   is
      Rew : C_Source_Rewriter;
   begin
      Rew.Start_Rewriting (Info, Filename, Self);
      Auto_Dump_Buffers_In_Main (IC, Info, Main, Rew, Self);
      Rew.Apply;
   end Auto_Dump_Buffers_In_Main;

   -------------------------
   -- Format_Str_Constant --
   -------------------------

   function Format_Str_Constant (Value : String) return String is
   begin
      return "{""" & Value & """," & Value'Length'Image & "}";
   end Format_Str_Constant;

   ----------------
   -- Format_Def --
   ----------------

   function Format_Def
     (Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Func_Args    : String := "";
      Init_Expr    : String := "";
      External     : Boolean := False) return String
   is
      Result : Unbounded_String;
   begin
      if External then
         Append (Result, Instrumenter.Extern_Prefix);

      --  In C++, if we are requested to return a definition (Func_Args = ""),
      --  we must first emit a declaration in order to correctly set the
      --  C linkage.

      elsif Instrumenter.Language = CPP_Language and then Func_Args = "" then
         Append
           (Result,
              Format_Def
                (Instrumenter,
                 C_Type,
                 Name,
                 Array_Size => Array_Size,
                 External   => True));
         Append (Result, ASCII.LF);
      end if;

      Append (Result, C_Type);
      Append (Result, ' ');
      Append (Result, Name);
      if Array_Size /= "" then
         Append (Result, '[');
         Append (Result, Array_Size);
         Append (Result, ']');
      end if;
      if Func_Args /= "" then
         Append (Result, " (");
         Append (Result, Func_Args);
         Append (Result, ')');
      end if;
      if Init_Expr /= "" then
         Append (Result, " = ");
         Append (Result, Init_Expr);
      end if;
      Append (Result, ';');
      return To_String (Result);
   end Format_Def;

   --------------------
   -- Put_Format_Def --
   --------------------

   procedure Put_Format_Def
     (File         : in out Text_Files.File_Type;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Init_Expr    : String := "") is
   begin
      File.Put_Line
        (Format_Def
           (Instrumenter, C_Type, Name, Array_Size, Init_Expr => Init_Expr));
   end Put_Format_Def;

   ------------------------
   -- Format_Extern_Decl --
   ------------------------

   function Format_Extern_Decl
     (Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Func_Args    : String := "") return String is
   begin
      return
        Format_Def
          (Instrumenter,
           C_Type,
           Name,
           Array_Size => Array_Size,
           Func_Args  => Func_Args,
           External   => True);
   end Format_Extern_Decl;

   ---------------------
   -- Put_Extern_Decl --
   ---------------------

   procedure Put_Extern_Decl
     (Rewriter     : Rewriter_T;
      Location     : Source_Location_T;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Array_Size   : String := "";
      Func_Args    : String := "") is
   begin
      CX_Rewriter_Insert_Text_After
        (Rewriter,
         Location,
         Format_Extern_Decl (Instrumenter, C_Type, Name, Array_Size, Func_Args)
         & ASCII.LF);
   end Put_Extern_Decl;

   ---------------------
   -- Put_Extern_Decl --
   ---------------------

   procedure Put_Extern_Decl
     (File         : in out Text_Files.File_Type;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      C_Type       : String;
      Name         : String;
      Func_Args    : String := "") is
   begin
      File.Put_Line
        (Format_Extern_Decl (Instrumenter, C_Type, Name, Func_Args));
   end Put_Extern_Decl;

   --------------------------------
   -- Find_First_Insert_Location --
   --------------------------------

   function Find_First_Insert_Location
     (TU : Translation_Unit_T) return Source_Location_T
   is
      Location : Source_Location_T := Get_Null_Location;

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T;
      --  Callback for Visit_Children

      ----------------
      -- Visit_Decl --
      ----------------

      function Visit_Decl
        (Cursor : Cursor_T) return Child_Visit_Result_T is
      begin
         if Kind (Cursor) = Cursor_Translation_Unit then
            return Child_Visit_Recurse;
         end if;
         declare
            Cursor_Location : constant Source_Location_T :=
              Get_Range_Start (Get_Cursor_Extent (Cursor));
         begin
            if not Is_Macro_Location (Location) then
               Location := Cursor_Location;
               return Child_Visit_Break;
            end if;
         end;
         return Child_Visit_Continue;
      end Visit_Decl;

   --  Start of processing for Find_First_Insert_Location

   begin
      Visit_Children (Parent  => Get_Translation_Unit_Cursor (TU),
                      Visitor => Visit_Decl'Access);
      return Location;
   end Find_First_Insert_Location;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   overriding procedure Emit_Buffers_List_Unit
     (Self              : C_Family_Instrumenter_Type;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info)
   is
      Base_Filename  : constant String := Buffers_List_Filename (IC);
      CU_Name_Body   : constant String :=
        Base_Filename
        & Source_Suffix (Self, GPR.Unit_Body, Root_Project_Info.Project);
      CU_Name_Header : constant String :=
        Base_Filename
        & Source_Suffix (Self, GPR.Unit_Spec, Root_Project_Info.Project);

      File_Body   : Text_Files.File_Type;
      File_Header : Text_Files.File_Type;

      Instr_Units : CU_Name_Vectors.Vector;
   begin

      for Cur in IC.Instrumented_Units.Iterate loop
         declare
            Instr_Unit : constant Compilation_Unit_Name :=
              Instrumented_Unit_Maps.Key (Cur);
         begin
            Instr_Units.Append (Instr_Unit);
         end;
      end loop;

      declare
         Buffer_Array_Decl  : constant String :=
           "const struct gnatcov_rts_coverage_buffers_group_array "
           & Unit_Buffers_Array_Name (IC);
         Buffer_Unit_Length : constant String :=
           Count_Type'Image (Instr_Units.Length);
      begin
         --  Emit the body to contain the list of buffers

         Create_File (Root_Project_Info, File_Body, CU_Name_Body);

         File_Body.Put_Line ("#include ""gnatcov_rts_c-buffers.h""");

         --  First create extern declarations for the buffers group of each
         --  unit.

         for Instr_Unit of Instr_Units loop
            Put_Extern_Decl
              (File_Body,
               Self,
               "const struct gnatcov_rts_coverage_buffers_group",
               Unit_Buffers_Name (Instr_Unit));
         end loop;

         --  Then create an extern declaration for the buffer array (necessary
         --  in C++ to set the C linkage), and finally the definition for that
         --  array.

         File_Body.Put_Line (Self.Extern_Prefix & Buffer_Array_Decl & ";");
         File_Body.Put_Line (Buffer_Array_Decl & " = {");
         File_Body.Put_Line ("  " & Buffer_Unit_Length & ",");
         File_Body.Put_Line
           ("  (const struct gnatcov_rts_coverage_buffers_group *[]) {");
         for Cur in Instr_Units.Iterate loop
            declare
               use CU_Name_Vectors;
            begin
               File_Body.Put ("    &" & Unit_Buffers_Name (Element (Cur)));
               if To_Index (Cur) = Instr_Units.Last_Index then
                  File_Body.Put_Line ("}};");
               else
                  File_Body.Put_Line (",");
               end if;
            end;
         end loop;
      end;

      --  Emit the extern declaration of the buffers array in the header file

      Create_File (Root_Project_Info, File_Header, CU_Name_Header);

      Put_Extern_Decl
        (File_Header,
         Self,
         "const struct gnatcov_rts_coverage_buffers_group_array",
         Unit_Buffers_Array_Name (IC));
   end Emit_Buffers_List_Unit;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   overriding procedure Instrument_Unit
     (Self      : C_Family_Instrumenter_Type;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info)
   is
      Prj_Info : Project_Info renames Unit_Info.Prj_Info.all;
      UIC      : C_Unit_Inst_Context;
   begin
      Instrument_Source_File
        (CU_Name      => CU_Name,
         Unit_Info    => Unit_Info,
         Instrumenter => Self,
         Prj_Info     => Prj_Info,
         IC           => IC,
         UIC          => UIC);

      --  Generate a buffer compilation unit defining coverage buffers that
      --  will store execution witnesses. This CU is a C file rather than an
      --  Ada file exporting the defined symboled to C. Indeed, we want it to
      --  be compatible with a C-only compiler.

      Emit_Buffer_Unit (Prj_Info, UIC, Self);
   end Instrument_Unit;

   ----------------------
   -- Skip_Source_File --
   ----------------------

   overriding function Skip_Source_File
     (Self        : C_Family_Instrumenter_Type;
      Source_File : GNATCOLL.Projects.File_Info) return Boolean
   is
      use GNATCOLL.Projects;
   begin
      --  Do not instrument C headers: code in C header is meant to be
      --  instrumented at the time it is included in a ".c" source.

      return Source_File.Unit_Part = Unit_Spec;
   end Skip_Source_File;

   -----------------
   -- Add_Options --
   -----------------

   procedure Add_Options
     (Args          : in out String_Vectors.Vector;
      Options       : Analysis_Options;
      Pass_Builtins : Boolean := True) is

      procedure Add_Macro_Switches (Macros : Macro_Vectors.Vector);
      --  Add the given macro switches to Args

      ------------------------
      -- Add_Macro_Switches --
      ------------------------

      procedure Add_Macro_Switches (Macros : Macro_Vectors.Vector) is
      begin
         for M of Macros loop
            declare
               Prefix : constant Unbounded_String :=
                 +(if M.Define
                   then "-D"
                   else "-U");
            begin
               Args.Append (Prefix & M.Value);
            end;
         end loop;
      end Add_Macro_Switches;

   begin
      for Dir of Options.PP_Search_Path loop
         Args.Append (+"-I");
         Args.Append (Dir);
      end loop;

      --  Add builtin macros before macros from command line switches, as the
      --  latter should have precedence over builtins and thus must come last
      --  in Args.

      if Pass_Builtins then
         Add_Macro_Switches (Options.Builtin_Macros);
      end if;
      Add_Macro_Switches (Options.PP_Macros);

      --  The -std switch also indicates the C/C++ version used, and
      --  influences both the configuration of the preprocessor, and the
      --  parsing of the file.

      if Length (Options.Std) /= 0 then
         Args.Append (Options.Std);
      end if;

   end Add_Options;

   -------------------------
   -- Import_From_Project --
   -------------------------

   procedure Import_From_Project
     (Self     : out Analysis_Options;
      Language : C_Family_Language;
      Info     : Project_Info;
      Filename : String)
   is
      Switches : GNAT.Strings.String_List_Access;
   begin
      --  Pass the source directories of the project file as -I options

      for Dir of Info.Project.Source_Dirs loop
         Self.PP_Search_Path.Append
           (+GNATCOLL.VFS."+" (GNATCOLL.VFS.Dir_Name (Dir)));
      end loop;

      --  Now get actual compiler switches from the project file for Filename.
      --  First try to get the switches specifically for Filename, then if
      --  there are none fall back to default switches for C.

      Switches :=
        Info.Project.Attribute_Value
          (Attribute => GPR.Build ("compiler", "switches"),
           Index     => Simple_Name (Filename));

      if Switches = null then
         Switches :=
           Info.Project.Attribute_Value
             (Attribute => GPR.Compiler_Default_Switches_Attribute,
              Index     => Image (Language));
      end if;

      --  If we manage to find appropriate switches, convert them to a string
      --  vector import the switches.

      if Switches /= null then
         declare
            Args : String_Vectors.Vector;
         begin
            for S of Switches.all loop
               Args.Append (To_Unbounded_String (S.all));
            end loop;
            GNAT.Strings.Free (Switches);
            Import_From_Args (Self, Args);
         end;
      end if;
   end Import_From_Project;

   ----------------------
   -- Import_From_Args --
   ----------------------

   procedure Import_From_Args
     (Self : in out Analysis_Options; Args : String_Vectors.Vector)
   is
      I    : Natural := Args.First_Index;
      Last : constant Integer := Args.Last_Index;

      function Read_With_Argument
        (Arg         : String;
         Option_Name : Character;
         Value       : out Unbounded_String) return Boolean;
      --  Assuming that Arg starts with "-X" where X is Option_Name, try to
      --  fetch the value for this option. If we managed to get one, return
      --  True and set Value to it. Return False otherwise.

      ------------------------
      -- Read_With_Argument --
      ------------------------

      function Read_With_Argument
        (Arg         : String;
         Option_Name : Character;
         Value       : out Unbounded_String) return Boolean
      is
         Prefix : constant String := "-" & Option_Name;
      begin
         if Arg = Prefix then

            --  Option and value are two separate arguments (-O VALUE)

            I := I + 1;
            if I <= Last then
               Value := Args (I);
               return True;
            end if;

         elsif Has_Prefix (Arg, Prefix) then

            --  Option and value are combined in a single argument (-OVALUE)

            Value := +Arg (Arg'First + Prefix'Length .. Arg'Last);
            return True;
         end if;

         return False;
      end Read_With_Argument;

   --  Start of processing for Import_From_Args

   begin
      while I <= Last loop
         declare
            A     : constant String := +Args (I);
            Value : Unbounded_String;
         begin

            --  Process arguments we manage to handle, silently discard unknown
            --  ones.
            --
            --  TODO??? In order to avoid surprising situations for users (for
            --  instance typos in command line arguments), maybe we should emit
            --  a warning for unknown arguments. However, given that this
            --  procedure is called at least once per instrumented source file,
            --  we would need to avoid emitting duplicate warnings.

            if Read_With_Argument (A, 'I', Value) then
               Self.PP_Search_Path.Append (Value);

            elsif Read_With_Argument (A, 'D', Value) then
               Self.PP_Macros.Append ((Define => True, Value => Value));

            elsif Read_With_Argument (A, 'U', Value) then
               Self.PP_Macros.Append ((Define => False, Value => Value));

            elsif Has_Prefix (A, "-std=") then
               Self.Std := +A;
            end if;

            I := I + 1;
         end;
      end loop;
   end Import_From_Args;

   ----------------
   -- Split_Args --
   ----------------

   function Split_Args (Args : Unbounded_String) return String_Vectors.Vector
   is
      Last_Is_Backslash : Boolean := False;
      --  Whether the last character analyzed was a backslash
   begin
      return Result : String_Vectors.Vector do

         --  The split of an empty string must yield a list of 1 empty string

         Result.Append (Null_Unbounded_String);

         for I in 1 .. Length (Args) loop
            declare
               C : constant Character := Element (Args, I);
            begin
               if Last_Is_Backslash then

                  --  If the last character was a backslash, we expect a comma
                  --  or a backslash. If we have something else, just treat
                  --  the backslash as a regular character, i.e. add the
                  --  backslash *and* the current character.
                  --
                  --  Note that this is to mimic the behavior of "sh", which is
                  --  convenient on Windows: "C:\foo" is valid and interpreted
                  --  as "C:\foo" (backslash preserved) instead of as, for
                  --  instance "C:foo" (backslash just ignored).

                  declare
                     Last_Arg : Unbounded_String
                       renames Result.Reference (Result.Last);
                  begin
                     case C is
                        when '\' | ',' =>
                           Append (Last_Arg, C);
                        when others =>
                           Append (Last_Arg, (1 => '\', 2 => C));
                     end case;
                  end;
                  Last_Is_Backslash := False;

               else
                  case C is
                     when '\' =>
                        Last_Is_Backslash := True;
                     when ',' =>
                        Result.Append (Null_Unbounded_String);
                     when others =>
                        Append (Result.Reference (Result.Last), C);
                  end case;
               end if;
            end;
         end loop;

         --  If we got a trailing backslash, treat it as a regular character

         if Last_Is_Backslash then
            Append (Result.Reference (Result.Last), '\');
         end if;
      end return;
   end Split_Args;

   --------------------
   -- Import_Options --
   --------------------

   procedure Import_Options
     (Self     : out Analysis_Options;
      Language : C_Family_Language;
      Info     : Project_Info;
      Filename : String)
   is
      Opt : constant Command_Line.String_List_Options :=
        (case Language is
         when C_Language   => Command_Line.Opt_C_Opts,
         when CPP_Language => Command_Line.Opt_CPP_Opts);
   begin
      Import_From_Project (Self, Language, Info, Filename);
      for Args of Switches.Args.String_List_Args (Opt) loop
         Import_From_Args (Self, Split_Args (Args));
      end loop;

      --  Now, we can generate the preprocessor configuration (i.e. the set
      --  of predefined macros).

      Self.Builtin_Macros :=
        Builtin_Macros
          (Image (Language),
           Compiler_Driver (Info.Project, Language),
           +Self.Std,
           +Info.Output_Dir).all;
   end Import_Options;

   ---------------------------
   -- Is_Source_Of_Interest --
   ---------------------------

   function Is_Source_Of_Interest
     (UIC : in out C_Unit_Inst_Context; N : Cursor_T) return Boolean
   is
      --  Determine the file from which N originates

      C_File : aliased String_T;
      Line   : aliased unsigned;
      Column : aliased unsigned;
      Loc    : constant Source_Location_T := Get_Cursor_Location (N);
      File   : Unbounded_String;
   begin
      Get_Presumed_Location (Location => Loc,
                             Filename => C_File'Access,
                             Line     => Line'Access,
                             Column   => Column'Access);
      File := +Get_C_String (C_File);

      --  Look for a corresponding entry in UIC.Sources_Of_Interest, create one
      --  if it is missing.

      declare
         use Source_Of_Interest_Maps;

         Cur : constant Cursor := UIC.Sources_Of_Interest.Find (File);
         SOI : Source_Of_Interest;
      begin
         if Has_Element (Cur) then
            return UIC.Sources_Of_Interest.Reference (Cur).Of_Interest;
         end if;

         if File = UIC.File then
            SOI :=
              (Of_Interest  => True,
               SFI          => Get_Index_From_Generic_Name
                                 (+File, Source_File),
               CU_Name      =>
                 CU_Name_For_File
                   (Filename     => +Simple_Name (+File),
                    Project_Name => UIC.Instrumented_Unit.Project_Name));
         else
            SOI := (Of_Interest => False);
         end if;
         UIC.Sources_Of_Interest.Insert (File, SOI);

         return SOI.Of_Interest;
      end;
   end Is_Source_Of_Interest;

end Instrument.C;
