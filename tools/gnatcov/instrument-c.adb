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

with Ada.Containers;  use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Directories; use Ada.Directories;
with Ada.Strings;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;     use Ada.Text_IO;

with Clang.Extensions; use Clang.Extensions;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Strings;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Command_Line;
with Coverage;            use Coverage;
with Coverage_Options;
with Files_Table;         use Files_Table;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with Hex_Images;          use Hex_Images;
with Instrument.C_Utils;  use Instrument.C_Utils;
with Outputs;             use Outputs;
with Paths;               use Paths;
with SCOs;
with Slocs;
with Subprocesses;        use Subprocesses;
with Switches;            use Switches;
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
     (Project : GNATCOLL.Projects.Project_Type) return String;
   --  Return the command name for the C compiler in the given Project

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
     (Compiler, Output_Dir : String) return Macro_Vector_Cst_Access;
   --  Return the list of built-in macros for the given compiler. Output_Dir
   --  is used to store a temporary file.

   procedure Preprocess_Source
     (Filename    : String;
      PP_Filename : out Unbounded_String;
      Info        : in out Project_Info;
      Options     : in out Analysis_Options);
   --  Preprocess the source at Filename and extend Options using the
   --  preprocessor output.
   --
   --  This uses the compiler in the Compiler_Driver project attribute to
   --  preprocess the file, assuming that it accepts the -E flag, to preprocess
   --  a file.

   ---------------------------
   --  Passes specificities --
   ---------------------------

   type Record_PP_Info_Pass_Kind is new Pass_Kind with null record;

   procedure Append_SCO
     (Pass               : Record_PP_Info_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a SCO to SCOs.SCO_Table. Also partially fill the preprocessing
   --  info: the actual source range referred, and the expanded macro name, if
   --  this is a SCO inside a macro expansion.

   type Instrument_Pass_Kind is new Pass_Kind with null record;

   procedure Append_SCO
     (Pass               : Instrument_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a SCO to SCOs.SCO_Table, and complete the preprocessing info with
   --  the preprocessed source range.

   procedure Instrument_Statement
     (Pass         : Instrument_Pass_Kind;
      UIC          : in out C_Unit_Inst_Context'Class;
      LL_SCO       : Nat;
      Insertion_N  : Cursor_T;
      Instr_Scheme : Instr_Scheme_Type);
   --  Add an entry to UIC.Source_Statements

   procedure Instrument_Decision
     (Pass     : Instrument_Pass_Kind;
      UIC      : in out C_Unit_Inst_Context'Class;
      LL_SCO   : Nat;
      Decision : Cursor_T;
      State    : US.Unbounded_String);
   --  Add an entry to UIC.Source_Decisions

   procedure Instrument_Condition
     (Pass      : Instrument_Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : US.Unbounded_String;
      First     : Boolean);
   --  Add an entry to UIC.Source_Conditions

   procedure Curlify
     (Pass : Instrument_Pass_Kind;
      N    : Cursor_T;
      Rew  : Rewriter_T);
   --  Wrapper around Instrument.C.Utils.Curlify

   procedure Insert_MCDC_State
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

   function Make_Expr_Witness
     (UIC : C_Unit_Inst_Context; Bit : Bit_Id) return String;
   --  Create a procedure call expression on to witness execution of the low
   --  level SCO with the given bit id.

   function Make_Statement_Witness
     (UIC : C_Unit_Inst_Context; Bit : Bit_Id) return String;
   --  Create a procedure call statement to witness execution of the low level
   --  SCO with the given bit id.

   procedure Insert_Statement_Witness
     (UIC : in out C_Unit_Inst_Context; SS : C_Source_Statement);
   --  Insert witness function call for the identified statement

   procedure Insert_Decision_Witness
     (UIC        : in out C_Unit_Inst_Context;
      SD         : C_Source_Decision;
      Path_Count : Positive);
   --  For use when decision coverage or MC/DC is requested. Insert witness
   --  function call for the identified decision.

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
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out C_Unit_Inst_Context);
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
   --  gnatcov_rts_unit_coverage_buffers_array struct (defined for the whole
   --  project). This struct is an array containing the coverage buffers of all
   --  of the instrumented units.
   --
   --  We need this to be unique per root project instrumented, as gnatcov
   --  gives the possibility to link two separately-instrumented libraries in
   --  the same executable.

   function Buffers_List_Filename (IC : Inst_Context) return String is
     ("gnatcov_rts_c-buffers-lists-" & (+IC.Project_Name));
   --  Return the name of the unit containing the array of coverage buffers

   function Buffers_List_Filename_Body (IC : Inst_Context) return String is
     (Buffers_List_Filename (IC) & ".c");
   --  Return the implementation filename body of the unit containing the
   --  array of coverage buffers.

   function Buffers_List_Filename_Header (IC : Inst_Context) return String is
     (Buffers_List_Filename (IC) & ".h");
   --  Return the implementation filename header of the unit containing the
   --  array of coverage buffers.

   procedure Emit_Buffer_Unit
     (Info : in out Project_Info; UIC : C_Unit_Inst_Context'Class);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit.

   procedure Emit_Dump_Helper_Unit
     (IC          : Inst_Context;
      Info        : in out Project_Info;
      Main        : Compilation_Unit_Name;
      Helper_Unit : out US.Unbounded_String);
   --  Emit the unit to contain helpers to implement the automatic dump of
   --  coverage buffers for the given Main unit. Info must be the project that
   --  owns this main. Upon return, the name of this helper unit is stored in
   --  Helper_Unit.

   procedure Run_Diagnostics (TU : Translation_Unit_T)
   with Unreferenced;
   --  Output clang diagnostics on the given translation unit
   --
   --  TODO??? If this is a debug helper, use it in verbose mode instead of
   --  leaving it unreferenced.

   procedure Auto_Dump_Buffers_In_Main
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : in out C_Source_Rewriter);
   --  Common code for auto dump insertion in the "main" function, used in the
   --  Auto_Dump_Buffers_In_Main primitive for C_Instrumenter_Type, and from
   --  the Instrument_Source_File procedure.
   --
   --  Arguments have the same semantics as in the Auto_Dump_Buffers_In_Main
   --  primitive. The additional Rew argument is the C source rewriter that is
   --  ready to use for the source file to instrument.

   function Format_Def
     (C_Type     : String;
      Name       : String;
      Array_Size : String := "";
      Func_Args  : String := "";
      Init_Expr  : String := "";
      External   : Boolean := False) return String;
   --  Helper to format a variable/constant definition or declaration.
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
     (File       : in out Text_Files.File_Type;
      C_Type     : String;
      Name       : String;
      Array_Size : String := "";
      Init_Expr  : String := "");
   --  Like Format_Def, but write the definition to File

   function Format_Extern_Decl
     (C_Type    : String;
      Name      : String;
      Func_Args : String := "") return String;
   --  Helper for format an "extern" declaration. Arguments are the same as
   --  Format_Def.

   procedure Put_Extern_Decl
     (Rewriter  : Rewriter_T;
      Location  : Source_Location_T;
      C_Type    : String;
      Name      : String;
      Func_Args : String := "");
   --  Like Format_Extern_Decl, but write the definition to TU/Rewriter

   procedure Put_Extern_Decl
     (File      : in out Text_Files.File_Type;
      C_Type    : String;
      Name      : String;
      Func_Args : String := "");
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
     (Project : GNATCOLL.Projects.Project_Type) return String
   is
   begin
      return GNATCOLL.Projects.Attribute_Value
               (Project, GPR.Compiler_Driver_Attribute, "C");
   end Compiler_Driver;

   ----------------
   -- Append_SCO --
   ----------------

   procedure Append_SCO
     (Pass               : Record_PP_Info_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
      Loc : Source_Location_T :=
        Get_Range_Start (Get_Cursor_Extent (N));

      Line, Column, Offset : aliased unsigned;
      File                 : File_T;
      Info                 : PP_Info;
   begin
      Append_SCO (C1, C2, From, To, Last, Pragma_Aspect_Name);

      --  We add preprocessing information only for actual SCOs. Return there
      --  if this is an operator SCO or a dominance SCO.

      if C1 in '>' | '!' | '&' | '|' then
         return;
      end if;

      Get_Expansion_Location
        (Loc, File'Address, Line'Access, Column'Access, Offset'Access);

      Info.Actual_Source_Range :=
        (First_Sloc =>
           (Line   => Integer (From.Line),
            Column => Integer (From.Column)),
         Last_Sloc  =>
           (Line   => Integer (To.Line),
            Column => Integer (To.Column)));

      --  Check if this is comes from a macro expansion, in which case we need
      --  to record some information, for reporting purposes.

      if Is_Macro_Location (Loc) then
         declare
            Expansion_Stack : Expansion_Lists.List;
            Definition_Info : Expansion_Info;

            Macro_Expansion_Name      : US.Unbounded_String;
            Immediate_Expansion_Loc_C : Source_Location_T;
            Immediate_Expansion_Loc   : Slocs.Source_Location;

            Macro_Arg_Expanded_Loc_C : aliased Source_Location_T;
            Macro_Arg_Expanded_Loc   : Slocs.Source_Location;
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

            if Is_Macro_Arg_Expansion
              (Loc, Macro_Arg_Expanded_Loc_C'Access, UIC.TU)
            then
               Get_Spelling_Location
                 (Macro_Arg_Expanded_Loc_C,
                  File'Address,
                  Line'Access,
                  Column'Access,
                  Offset'Access);
               Macro_Arg_Expanded_Loc :=
                 (Source_File =>
                    Get_Index_From_Generic_Name
                      (Name => Get_File_Name (File),
                       Kind => Source_File),
                  L           =>
                    (Line   => Integer (Line),
                     Column => Integer (Column)));

               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics
                    (Macro_Arg_Expanded_Loc_C, UIC.TU);

               Definition_Info :=
                 (Macro_Name => Macro_Expansion_Name,
                  Sloc       => Macro_Arg_Expanded_Loc);
            else
               Get_Spelling_Location
                 (Loc,
                  File'Address,
                  Line'Access,
                  Column'Access,
                  Offset'Access);

               Immediate_Expansion_Loc :=
                 (Source_File =>
                    Get_Index_From_Generic_Name
                      (Name => Get_File_Name (File),
                       Kind => Source_File),
                  L           =>
                    (Line   => Integer (Line),
                     Column => Integer (Column)));
               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics (Loc, UIC.TU);
               Definition_Info :=
                 (Macro_Name => Macro_Expansion_Name,
                  Sloc       => Immediate_Expansion_Loc);
            end if;

            while Is_Macro_Location (Loc) loop

               Immediate_Expansion_Loc_C := Loc;

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
                 (Immediate_Expansion_Loc_C,
                  Macro_Arg_Expanded_Loc_C'Access,
                  UIC.TU)
               loop
                  --  TODO??? Document why it is needed to loop while we are
                  --  in a macro argument expansion (did not manage to make an
                  --  example that looped several times). Note that this
                  --  strictly follows the implementation of
                  --  Get_Immediate_Macro_Name_For_Diagnostics implemented in
                  --  clang.

                  Immediate_Expansion_Loc_C :=
                    Get_Immediate_Expansion_Loc
                      (Immediate_Expansion_Loc_C, UIC.TU);
               end loop;

               --  Immediate_Expansion_Loc is the location of the token in
               --  the immediate expanded macro definition. To get to the
               --  expansion point, go up one level.

               Immediate_Expansion_Loc_C :=
                 Get_Immediate_Expansion_Loc
                   (Immediate_Expansion_Loc_C, UIC.TU);
               Get_Spelling_Location
                 (Immediate_Expansion_Loc_C,
                  File'Address,
                  Line'Access,
                  Column'Access,
                  Offset'Access);

               Immediate_Expansion_Loc :=
                 (Source_File =>
                    Get_Index_From_Generic_Name
                      (Name => Get_File_Name (File),
                       Kind => Source_File),
                  L           =>
                    (Line   => Integer (Line),
                     Column => Integer (Column)));
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
                      Sloc       => Immediate_Expansion_Loc));
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

   ----------------
   -- Append_SCO --
   ----------------

   procedure Append_SCO
     (Pass               : Instrument_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
   begin
      Append_SCO (C1, C2, From, To, Last, Pragma_Aspect_Name);

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

            Line, Column, Offset : aliased unsigned;
            File                 : File_T;

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
            --  Get start of the range

            Get_File_Location
              (Start_Loc,
               File'Address,
               Line'Access,
               Column'Access,
               Offset'Access);
            Cursor_Source_Range.First_Sloc :=
              (Line => Natural (Line), Column => Natural (Column));

            --  Get end of the range. Note: end column is exclusive

            Get_File_Location
              (End_Loc,
               File'Address,
               Line'Access,
               Column'Access,
               Offset'Access);

            Cursor_Source_Range.Last_Sloc :=
              (Line => Natural (Line), Column => Natural (Column) - 1);

            UIC.LL_PP_Info_Map.Update_Element
              (UIC.LL_PP_Info_Map.Find (SCOs.SCO_Table.Last), Update'Access);
         end;
      end if;
   end Append_SCO;

   -------------
   -- Curlify --
   -------------

   procedure Curlify
     (Pass : Instrument_Pass_Kind; N : Cursor_T; Rew : Rewriter_T) is
   begin
      Curlify (N, Rew);
   end Curlify;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   procedure Insert_MCDC_State
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

   procedure Instrument_Statement
     (Pass         : Instrument_Pass_Kind;
      UIC          : in out C_Unit_Inst_Context'Class;
      LL_SCO       : Nat;
      Insertion_N  : Cursor_T;
      Instr_Scheme : Instr_Scheme_Type) is
   begin
      UIC.Source_Statements.Append
        (C_Source_Statement'
           (LL_SCO       => SCOs.SCO_Table.Last,
            Instr_Scheme => Instr_Scheme,
            Statement    => Insertion_N));
   end Instrument_Statement;

   -------------------------
   -- Instrument_Decision --
   -------------------------

   procedure Instrument_Decision
     (Pass     : Instrument_Pass_Kind;
      UIC      : in out C_Unit_Inst_Context'Class;
      LL_SCO   : Nat;
      Decision : Cursor_T;
      State    : US.Unbounded_String) is
   begin
      UIC.Source_Decisions.Append
        (C_Source_Decision'
           (LL_SCO   => LL_SCO,
            Decision => Decision,
            State    => State));
   end Instrument_Decision;

   --------------------------
   -- Instrument_Condition --
   --------------------------

   procedure Instrument_Condition
     (Pass      : Instrument_Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : US.Unbounded_String;
      First     : Boolean) is
   begin
      UIC.Source_Conditions.Append
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
     (UIC : C_Unit_Inst_Context; Bit : Bit_Id) return String
   is
      Bit_Img : constant String  := Img (Bit);
   begin
      return "gnatcov_rts_witness ("
        & Statement_Buffer_Symbol (UIC.Instrumented_Unit) & "," & Bit_Img
        & ")";
   end Make_Expr_Witness;

   ----------------------------
   -- Make_Statement_Witness --
   ----------------------------

   function Make_Statement_Witness
     (UIC : C_Unit_Inst_Context; Bit : Bit_Id) return String is
   begin
      return Make_Expr_Witness (UIC, Bit) & ";";
   end Make_Statement_Witness;

   ------------------------------
   -- Insert_Statement_Witness --
   ------------------------------

   procedure Insert_Statement_Witness
     (UIC : in out C_Unit_Inst_Context; SS : C_Source_Statement)
   is
      --  Allocate a bit in the statement coverage buffer, and record
      --  its id in the bitmap.

      Bit : constant Bit_Id := UIC.Unit_Bits.Last_Statement_Bit + 1;
   begin
      UIC.Unit_Bits.Last_Statement_Bit := Bit;
      UIC.Unit_Bits.Statement_Bits.Append
        (Statement_Bit_Ids'(SS.LL_SCO, Executed => Bit));

      --  Insert the call to the witness function: as a foregoing statement if
      --  SS.Statement is a statement, or as a previous expression (using the
      --  comma operator) if SS.Statement is an expression.

      case SS.Instr_Scheme is
         when Instr_Stmt =>
            Insert_Text_After_Start_Of
              (N    => SS.Statement,
               Text => Make_Statement_Witness (UIC, Bit),
               Rew  => UIC.Rewriter);

         when Instr_Expr =>
            Insert_Text_After_Start_Of
              (N    => SS.Statement,
               Text => "(" & Make_Expr_Witness (UIC, Bit) & ", ",
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
            Text => "gnatcov_rts_witness_condition" & " ("
                    & US.To_String (SC.State) & ", " & Img (Offset) & ", "
                    & First_Image & ", ",
            Rew  => UIC.Rewriter);
         Insert_Text_Before_End_Of (N    => SC.Condition,
                                    Text => " ? 1 : 0)",
                                    Rew  => UIC.Rewriter);
      end;
   end Insert_Condition_Witness;

   -----------------------------
   -- Insert_Decision_Witness --
   -----------------------------

   procedure Insert_Decision_Witness
     (UIC        : in out C_Unit_Inst_Context;
      SD         : C_Source_Decision;
      Path_Count : Positive)
   is
      LL_SCO_Id : Nat renames SD.LL_SCO;
      N         : Cursor_T renames SD.Decision;

      Bits : Decision_Bit_Ids;

   begin
      Bits.LL_D_SCO := LL_SCO_Id;

      --  Allocate outcome bits

      Bits.Outcome_Bits :=
        (False => UIC.Unit_Bits.Last_Outcome_Bit + 1,
         True  => UIC.Unit_Bits.Last_Outcome_Bit + 2);
      UIC.Unit_Bits.Last_Outcome_Bit :=
        UIC.Unit_Bits.Last_Outcome_Bit + 2;

      --  Allocate path bits for MC/DC if MC/DC is required and we were
      --  able to generate a local state variable.

      if MCDC_Coverage_Enabled and then US.Length (SD.State) > 0 then
         Bits.Path_Bits_Base := UIC.Unit_Bits.Last_Path_Bit + 1;
         UIC.Unit_Bits.Last_Path_Bit :=
           UIC.Unit_Bits.Last_Path_Bit + Bit_Id (Path_Count);
      else
         Bits.Path_Bits_Base := No_Bit_Id;
      end if;

      UIC.Unit_Bits.Decision_Bits.Append (Bits);

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
                    & Decision_Buffer_Symbol (UIC.Instrumented_Unit) & ", "
                    & Img (Bits.Outcome_Bits (False)) & ", "
                    & Img (Bits.Outcome_Bits (True)),
            Rew  => UIC.Rewriter);

         if Is_MCDC then
            Insert_Text_After_Start_Of
              (N    => N,
               Text => ", " & MCDC_Buffer_Symbol (UIC.Instrumented_Unit) & ", "
                       & Img (Bits.Path_Bits_Base) & ", "
                       & US.To_String (SD.State),
               Rew  => UIC.Rewriter);
         end if;
         Insert_Text_After_Start_Of (N    => N,
                                     Text => ", ",
                                     Rew  => UIC.Rewriter);

         --  Wrap the decision inside a ternary expression so that we always
         --  pass an unsigned value to the witness function. This turns <dec>
         --  into (<dec>) ? 1 : 0.

         Insert_Text_Before_End_Of (N    => N,
                                    Text => " ? 1 : 0)",
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

      From : Source_Location;
      To   : Source_Location;
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

   type Dominant_Info is record
      K : Character;
      --  F/T/S/E for a valid dominance marker, or ' ' for no dominant

      N : Cursor_T;
      --  Node providing the Sloc(s) for the dominance marker
   end record;
   No_Dominant : constant Dominant_Info := (' ', Get_Null_Cursor);

   procedure Traverse_Statements
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant);
   --  Process L, a list of statements or declarations dominated by D

   function Traverse_Statements
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant) return Dominant_Info;
   --  Process L, a list of statements or declarations dominated by D. Returns
   --  dominant information corresponding to the last node with SCO in L.

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
         Sloc      : Source_Location;
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

      function Process_Node (N : Cursor_T) return Child_Visit_Result_T
        with Convention => C;
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
               To   => No_Source_Location,
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
      Visit (N, Process_Node'Unrestricted_Access);
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

      function Visitor (N : Cursor_T) return Child_Visit_Result_T
        with Convention => C;
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
         else
            return Child_Visit_Recurse;
         end if;
      end Visitor;

   --  Start of processing for Has_Decision

   begin
      Visit (T, Visitor'Unrestricted_Access);
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
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant)
   is
      Discard_Dom : constant  Dominant_Info :=
        Traverse_Statements (IC, UIC, L, D);
   begin
      null;
   end Traverse_Statements;

   function Traverse_Statements
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant) return Dominant_Info
   is
      Current_Dominant : Dominant_Info := D;

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;

      procedure Traverse_One (N : Cursor_T);
      --  Traverse a statement

      procedure Extend_Statement_Sequence
        (N           : Cursor_T;
         Typ         : Character;
         Insertion_N : Cursor_T := Get_Null_Cursor;
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
               Current_Dominant := No_Dominant;

            --  Compound statement, which breaks the current statement sequence

            when Cursor_Compound_Stmt =>
               Set_Statement_Entry;
               Traverse_Statements
                 (IC, UIC,
                  L   => Get_Children (N),
                  D   => Current_Dominant);

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
                     L => To_Vector (Then_Part),
                     D => ('S', N));

                  --  Traverse the ELSE statements if present

                  if not Is_Null (Else_Part) then
                     UIC.Pass.Curlify (N   => Else_Part,
                                       Rew => UIC.Rewriter);
                     Traverse_Statements
                       (IC, UIC,
                        L => To_Vector (Else_Part),
                        D => ('S', N));
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

                  --  Process case branches, all of which are dominated by the
                  --  Switch statement.

                  Traverse_Statements
                    (IC, UIC,
                     L => To_Vector (Alt),
                     D => Current_Dominant);
               end;

            --  Case alternative

            when Cursor_Case_Stmt | Cursor_Default_Stmt =>
               declare
                  Case_Body : constant Cursor_T := Get_Sub_Stmt (N);
               begin
                  Traverse_Statements
                    (IC, UIC,
                     L => To_Vector (Case_Body),
                     D => Current_Dominant);
               end;

            --  Loop ends the current statement sequence, but we include
            --  the iteration scheme if present in the current sequence.
            --  But the body of the loop starts a new sequence, since it
            --  may not be executed as part of the current sequence.

            when Cursor_While_Stmt =>
               declare
                  While_Body     : constant Cursor_T := Get_Body (N);
                  Cond           : constant Cursor_T := Get_Cond (N);
                  Inner_Dominant : constant Dominant_Info := ('S', N);

               begin
                  UIC.Pass.Curlify (N   => While_Body,
                                    Rew => UIC.Rewriter);
                  Extend_Statement_Sequence
                    (N, 'W',
                     Insertion_N  => Cond,
                     Instr_Scheme => Instr_Expr);
                  Process_Decisions_Defer (Cond, 'W');
                  Set_Statement_Entry;
                  Traverse_Statements
                    (IC, UIC, To_Vector (While_Body), Inner_Dominant);
               end;

            --  Do while statement. Ends the current statement sequence.

            when Cursor_Do_Stmt =>
               declare
                  Do_Body  : constant Cursor_T := Get_Body (N);
                  Do_While : constant Cursor_T := Get_Cond (N);

               begin
                  UIC.Pass.Curlify (N   => Do_Body,
                                    Rew => UIC.Rewriter);

                  Traverse_Statements
                    (IC, UIC, To_Vector (Do_Body), No_Dominant);
                  Extend_Statement_Sequence
                    (Do_While, 'W', Instr_Scheme => Instr_Expr);

                  Process_Decisions_Defer (Do_While, 'W');
                  Set_Statement_Entry;
                  Current_Dominant := ('S', Do_While);

               end;

            --  For statement. Ends the current statement sequence.

            when Cursor_For_Stmt =>
               declare
                  For_Init : constant Cursor_T := Get_For_Init (N);
                  For_Cond : constant Cursor_T := Get_Cond (N);
                  For_Inc  : constant Cursor_T := Get_For_Inc (N);
                  For_Body : constant Vector := To_Vector (Get_Body (N));
               begin
                  Extend_Statement_Sequence
                    (For_Init, ' ', Insertion_N => N);
                  Extend_Statement_Sequence
                    (For_Cond, 'F', Instr_Scheme => Instr_Expr);

                  --  The guard expression for the FOR loop is a decision. The
                  --  closest match for this kind of decision is a while loop.

                  Process_Decisions_Defer (For_Cond, 'W');

                  Set_Statement_Entry;

                  --  The first statement that is nested in the FOR loop runs
                  --  iff the guard expression evaluates to True. Set the
                  --  dominant accordingly.

                  Current_Dominant := ('T', For_Cond);

                  Current_Dominant :=
                    Traverse_Statements (IC, UIC, For_Body, Current_Dominant);

                  Extend_Statement_Sequence
                    (For_Inc, ' ', Instr_Scheme => Instr_Expr);

                  Set_Statement_Entry;

                  --  Evaluation of the guard expression necessarily precedes
                  --  evaluation of the first statement that follows the
                  --  FOR loop.

                  Current_Dominant := ('S', For_Cond);
               end;

           --  Unconditional goto, which is included in the current statement
           --  sequence, but then terminates it.

            when Cursor_Goto_Stmt | Cursor_Indirect_Goto_Stmt =>
               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            when Cursor_Label_Stmt =>
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;
               Traverse_Statements (IC, UIC, Get_Children (N));

            when Cursor_Stmt_Expr =>
               Traverse_Statements (IC, UIC, Get_Children (N));

            --  Null statement, we won't monitor their execution

            when Cursor_Null_Stmt =>
               null;

            --  TODO??? there are probably missing special statements, such as
            --  ternary operator etc. Do that in a later step.

            when others =>
               if Is_Declaration (Kind (N)) then
                  Traverse_Statements
                    (IC => IC,
                     UIC => UIC,
                     L => Get_Children (N));
               else

                  --  Determine required type character code, or ASCII.NUL if
                  --  no SCO should be generated for this node.

                  Extend_Statement_Sequence (N, ' ');

                  --  Process any embedded decisions

                  if Has_Decision (N) then
                     Process_Decisions_Defer (N, 'X');
                  end if;
               end if;
         end case;
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

         F : constant Source_Location := Start_Sloc (N);
         T : Source_Location := End_Sloc (N);
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

         if Is_Null (N) or else not Is_Unit_Of_Interest (N, +UIC.File) then
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
            --  If there is a pending dominant for this statement sequence,
            --  emit a SCO for it.

            if J = SC_First and then Current_Dominant /= No_Dominant then
               declare
                  From : constant Source_Location :=
                    Start_Sloc (Current_Dominant.N);
                  To   : constant Source_Location :=
                    End_Sloc (Current_Dominant.N);

               begin
                  UIC.Pass.Append_SCO
                    (UIC  => UIC,
                     N    => Current_Dominant.N,
                     C1   => '>',
                     C2   => Current_Dominant.K,
                     From => From,
                     To   => To,
                     Last => False);
               end;
            end if;
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

         --  Last statement of basic block, if present, becomes new current
         --  dominant.

         if SC_Last >= SC_First then
            Current_Dominant := ('S', SC.Table (SC_Last).N);
         end if;

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
      return Current_Dominant;
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
   begin
      for N of L loop

         --  Only traverse the function declarations that belong to a unit of
         --  interest.

         begin
            if Kind (N) = Cursor_Function_Decl
               and then Is_Unit_Of_Interest (N, +UIC.File)
            then
               declare
                  --  Get_Body returns a Compound_Stmt, convert it to a list of
                  --  statements using the Get_Children utility.

                  Fun_Body : constant Cursor_Vectors.Vector :=
                    Get_Children (Get_Body (N));
               begin
                  if Fun_Body.Length > 0 then
                     UIC.MCDC_State_Declaration_Node := Fun_Body.First_Element;
                     Traverse_Statements (IC, UIC, Fun_Body);
                  end if;
               end;
            end if;
         end;
      end loop;
   end Traverse_Declarations;

   --------------------
   -- Builtin_Macros --
   --------------------

   function Builtin_Macros
     (Compiler, Output_Dir : String) return Macro_Vector_Cst_Access
   is
      use Compiler_Macros_Maps;

      Key    : constant Unbounded_String := +Compiler;
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

            Args.Append (+"-E");
            Args.Append (+"-dM");
            Args.Append (+"-");

            Run_Command
              (Command             => Compiler,
               Arguments           => Args,
               Origin_Command_Name =>
                 "getting built-in macros for " & Compiler,
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
     (Filename    : String;
      PP_Filename : out Unbounded_String;
      Info        : in out Project_Info;
      Options     : in out Analysis_Options)
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

      Cmd := (Command => +Compiler_Driver (Info.Project), others => <>);

      Append_Arg (Cmd, "-E");
      Add_Options (Cmd.Arguments, Options);

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

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self         : out C_Source_Rewriter;
      Info         : in out Project_Info;
      Filename     : String;
      Preprocessed : Boolean := False)
   is
      PP_Filename : Unbounded_String := +Filename;

      Options : Analysis_Options;
      Args    : String_Vectors.Vector;
   begin
      Import_Options (Options, Info, Filename);
      if not Preprocessed then
         Preprocess_Source (Filename, PP_Filename, Info, Options);
      end if;

      Self.CIdx :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      Add_Options (Args, Options);
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

      use String_Vectors;
   begin
      UIC.Pass := Record_PP_Info_Pass'Access;
      UIC.CIdx :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      --  Get the predefined macros and search paths of the user's compiler and
      --  inhibit the use of clang predefined macros. We want to fully emulate
      --  the user's preprocessor.

      Add_Options (Args, UIC.Options);
      Append (Args, +"-undef");
      Append (Args, +"-nostdinc");

      --  Convert to C compatible char**

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
              Options               => 0);
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
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out C_Unit_Inst_Context)
   is
      Orig_Filename : constant String  := +Unit_Info.Filename;
      PP_Filename   : Unbounded_String;
      --  Respectively original, and preprocessed filename

      Buffer_Filename : constant String :=
        To_Symbol_Name (Sys_Buffers) & "_b_" & Instrumented_Unit_Slug (CU_Name)
        & ".c";
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
        (C_Type    : String;
         Name      : String;
         Func_Args : String := "");
      --  Local shortcut to avoid passing UIC.TU/UIC.Rewriter explicitly

      ---------------------
      -- Put_Extern_Decl --
      ---------------------

      procedure Put_Extern_Decl
        (C_Type    : String;
         Name      : String;
         Func_Args : String := "") is
      begin
         Put_Extern_Decl
           (UIC.Rewriter, Insert_Extern_Location, C_Type, Name, Func_Args);
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

      --  Import analysis options for the file to preprocess, then run the
      --  preprocessor.

      Import_Options (UIC.Options, Prj_Info, Orig_Filename);
      Preprocess_Source (Orig_Filename, PP_Filename, Prj_Info, UIC.Options);

      --  Start by recording preprocessing information

      Record_PP_Info (Unit_Info, IC, UIC);

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
                       Preprocessed => True);
      UIC.TU := Rewriter.TU;
      UIC.Rewriter := Rewriter.Rewriter;
      Insert_Extern_Location := Find_First_Insert_Location (UIC.TU);

      Traverse_Declarations
        (IC  => IC,
         UIC => UIC,
         L   => Get_Children (Get_Translation_Unit_Cursor (UIC.TU)));

      SCOs.SCO_Unit_Table.Append
        ((File_Name  => new String'(Orig_Filename),
          File_Index => UIC.SFI,
          Dep_Num    => 1,
          From       => SCOs.SCO_Table.First,
          To         => SCOs.SCO_Table.Last));

      --  Check whether there is a mismatch between Last_SCO and
      --  SCOs.SCO_Table.Last. If there is, warn the user and discard the
      --  preprocessed info: the SCO mapping will be wrong.
      --  Note that this will result on possibly wrong column numbering in
      --  the report (as a macro expansion may offset a block of code by
      --  an arbitrary amount).

      if Record_PP_Info_Last_SCO /= SCOs.SCO_Table.Last then
         Outputs.Warn
           (Orig_Filename & ": preprocessed file coverage obligations"
              &  " inconsistent with obligations from the original file."
              & " Discarding preprocessing information.");
         UIC.LL_PP_Info_Map.Clear;
      end if;

      --  Convert low level SCOs from the instrumenter to high level SCOs.
      --  This creates BDDs for every decision.

      declare
         SCO_Map       : aliased LL_HL_SCO_Map :=
           (SCOs.SCO_Table.First .. SCOs.SCO_Table.Last => No_SCO_Id);
         Bit_Maps      : CU_Bit_Maps;
         Created_Units : Created_Unit_Maps.Map;
      begin
         Process_Low_Level_SCOs
           (Provider      => SC_Obligations.Instrumenter,
            Origin        => UIC.SFI,
            Created_Units => Created_Units,
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

         --  In the instrumentation case, the origin of SCO information is
         --  the original source file.

         UIC.CU := Created_Units.Element (UIC.SFI);

         for SS of UIC.Source_Statements loop
            Insert_Statement_Witness (UIC, SS);
         end loop;

         if Coverage.Enabled (Coverage_Options.Decision)
            or else MCDC_Coverage_Enabled
         then
            for SD of UIC.Source_Decisions loop
               Insert_Decision_Witness
                 (UIC, SD, Path_Count (SCO_Map (SD.LL_SCO)));
            end loop;

            if MCDC_Coverage_Enabled then

               --  As high-level SCO tables have been populated, we have built
               --  BDDs for each decisions, and we can now set the correct
               --  MC/DC path offset for each condition.

               for SC of UIC.Source_Conditions loop
                  Insert_Condition_Witness
                    (UIC, SC, Offset_For_True (SCO_Map (SC.LL_SCO)));
               end loop;
            end if;
         end if;

         Bit_Maps :=
           (Statement_Bits => new Statement_Bit_Map'
              (Bit_Id'First .. UIC.Unit_Bits.Last_Statement_Bit => No_SCO_Id),
            Decision_Bits  => new Decision_Bit_Map'
              (Bit_Id'First .. UIC.Unit_Bits.Last_Outcome_Bit =>
                   (No_SCO_Id, False)),
            MCDC_Bits      =>
               new MCDC_Bit_Map'(Bit_Id'First .. UIC.Unit_Bits.Last_Path_Bit =>
                                     (No_SCO_Id, 0)));

         for S_Bit_Alloc of UIC.Unit_Bits.Statement_Bits loop
            Bit_Maps.Statement_Bits (S_Bit_Alloc.Executed) :=
              SCO_Map (S_Bit_Alloc.LL_S_SCO);
         end loop;

         for D_Bit_Alloc of UIC.Unit_Bits.Decision_Bits loop
            declare
               D_SCO : constant SCO_Id := SCO_Map (D_Bit_Alloc.LL_D_SCO);
            begin
               for Outcome in Boolean loop
                  Bit_Maps.Decision_Bits
                    (D_Bit_Alloc.Outcome_Bits (Outcome)) := (D_SCO, Outcome);
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

         Set_Bit_Maps (UIC.CU, Bit_Maps);
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

      Put_Extern_Decl
        ("unsigned char *",
         Statement_Buffer_Symbol (UIC.Instrumented_Unit));
      Put_Extern_Decl
        ("unsigned char *",
         Decision_Buffer_Symbol (UIC.Instrumented_Unit));
      Put_Extern_Decl
        ("unsigned char *",
         MCDC_Buffer_Symbol (UIC.Instrumented_Unit));
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
           (IC   => IC,
            Info => Prj_Info,
            Main => UIC.Instrumented_Unit,
            Rew  => Rewriter);
      end if;
      Rewriter.Apply;
   end Instrument_Source_File;

   ----------------------
   -- Emit_Buffer_Unit --
   ----------------------

   procedure Emit_Buffer_Unit
     (Info : in out Project_Info; UIC : C_Unit_Inst_Context'Class)
   is
      CU_Name : Compilation_Unit_Name renames UIC.Buffer_Unit;
      File    : Text_Files.File_Type;

      --  As a reminder, the representation of a static-array variable differs
      --  from a pointer-to-array variable.

      Statement_Buffer : constant String :=
        Statement_Buffer_Symbol (UIC.Instrumented_Unit);
      Decision_Buffer  : constant String :=
        Decision_Buffer_Symbol (UIC.Instrumented_Unit);
      MCDC_Buffer      : constant String :=
        MCDC_Buffer_Symbol (UIC.Instrumented_Unit);

      Statement_Buffer_Repr : constant String :=
        "__" & Statement_Buffer_Symbol (UIC.Instrumented_Unit);
      Decision_Buffer_Repr  : constant String :=
        "__" & Decision_Buffer_Symbol (UIC.Instrumented_Unit);
      MCDC_Buffer_Repr      : constant String :=
        "__" & MCDC_Buffer_Symbol (UIC.Instrumented_Unit);
   begin
      Create_File (Info, File, +CU_Name.Filename);

      declare
         Fingerprint : Unbounded_String;

         Unit_Name : constant String :=
           To_Filename (Info.Project, UIC.Instrumented_Unit, C_Language);

         Project_Name : constant String := +UIC.Instrumented_Unit.Project_Name;

         --  Do not use 'Image so that we use the original casing for the
         --  enumerators, and thus avoid compilation warnings/errors.

         Statement_Last_Bit : constant String := Img
           (UIC.Unit_Bits.Last_Statement_Bit);
         Decision_Last_Bit  : constant String := Img
           (UIC.Unit_Bits.Last_Outcome_Bit);
         MCDC_Last_Bit      : constant String := Img
           (UIC.Unit_Bits.Last_Path_Bit);
      begin
         --  Turn the fingerprint value into the corresponding C literal (array
         --  of uint8_t).

         declare
            First : Boolean := True;
         begin
            Append (Fingerprint, "{");
            for Byte of SC_Obligations.Fingerprint (UIC.CU) loop
               if First then
                  First := False;
               else
                  Append (Fingerprint, ", ");
               end if;
               Append (Fingerprint, Strings.Img (Integer (Byte)));
            end loop;
            Append (Fingerprint, "}");
         end;

         File.Put_Line ("#include ""gnatcov_rts_c-buffers.h""");
         File.New_Line;
         Put_Format_Def
           (File,
            "unsigned char",
            Statement_Buffer_Repr,
            Array_Size =>
              Img (Any_Bit_Id'Max (1, UIC.Unit_Bits.Last_Statement_Bit + 1)));
         Put_Format_Def
           (File,
            "unsigned char *const",
            Statement_Buffer,
            Init_Expr => "&" & Statement_Buffer_Repr & "[0]");

         Put_Format_Def
           (File,
            "unsigned char",
            Decision_Buffer_Repr,
            Array_Size =>
              Img (Any_Bit_Id'Max (1, UIC.Unit_Bits.Last_Outcome_Bit + 1)));
         Put_Format_Def
           (File,
            "unsigned char *const",
            Decision_Buffer,
            Init_Expr => "&" & Decision_Buffer_Repr & "[0]");

         Put_Format_Def
           (File,
            "unsigned char",
            MCDC_Buffer_Repr,
            Array_Size =>
              Img (Any_Bit_Id'Max (1, UIC.Unit_Bits.Last_Path_Bit + 1)));
         Put_Format_Def
           (File,
            "unsigned char *const",
            MCDC_Buffer,
            Init_Expr => "&" & MCDC_Buffer_Repr & "[0]");

         Put_Format_Def
           (File,
            "struct gnatcov_rts_unit_coverage_buffers",
            Unit_Buffers_Name (UIC.Instrumented_Unit),
            Init_Expr =>
              "{"
              & ASCII.LF
              & "  .fingerprint = " & To_String (Fingerprint) & ","
              & ASCII.LF
              & "  .language_kind = FILE_BASED_LANGUAGE,"
              & ASCII.LF
              & "  .unit_part = NOT_APPLICABLE_PART,"
              & ASCII.LF
              & "  .unit_name = STR (""" & Unit_Name & """),"
              & ASCII.LF
              & "  .project_name = STR (""" & Project_Name & """),"
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

              & "  .statement_last_bit = " & Statement_Last_Bit & ","
              & ASCII.LF
              & "  .decision_last_bit = " & Decision_Last_Bit & ","
              & ASCII.LF
              & "  .mcdc_last_bit = " & MCDC_Last_Bit
              & ASCII.LF
              & "}");
      end;
   end Emit_Buffer_Unit;

   ---------------------------
   -- Emit_Dump_Helper_Unit --
   ---------------------------

   procedure Emit_Dump_Helper_Unit
     (IC          : Inst_Context;
      Info        : in out Project_Info;
      Main        : Compilation_Unit_Name;
      Helper_Unit : out US.Unbounded_String)
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

      Helper_Unit := +(To_Symbol_Name (Sys_Buffers) & "_d_"
                       & (+Main.Filename));

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
           ("#include """ & Buffers_List_Filename_Header (IC) & """");

         --  Emit the procedure to write the trace file

         File.New_Line;
         File.Put_Line ("void " & Dump_Procedure & " (void) {");

         File.Put_Line (Indent1 & Output_Proc & " (");
         File.Put_Line (Indent2 & "&" & Unit_Buffers_Array_Name (IC) & ",");
         case IC.Dump_Config.Channel is
         when Binary_File =>
            declare

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
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : in out C_Source_Rewriter)
   is
      Instr_Units : constant CU_Name_Vectors.Vector :=
        Instr_Units_For_Closure (IC, Main);
      --  List of names for instrumented units

      Helper_Filename : US.Unbounded_String;
      --  Name of file to contain helpers implementing the buffers dump

      Insert_Extern_Location : constant Source_Location_T :=
        Find_First_Insert_Location (Rew.TU);
      --  Where to insert extern declarations
   begin
      if Instr_Units.Is_Empty then
         return;
      end if;

      Emit_Dump_Helper_Unit (IC, Info, Main, Helper_Filename);
      Put_Extern_Decl
        (Rew.Rewriter,
         Insert_Extern_Location,
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
            Add_Statement_Before_Return
              (Fun_Decl  => Get_Main (Rew.TU),
               Rew       => Rew.Rewriter,
               Statement => Dump_Procedure_Symbol (Main) & "();");

         when At_Exit =>
            Put_Extern_Decl
              (Rew.Rewriter,
               Insert_Extern_Location,
               "int",
               "atexit",
               Func_Args => "void (*function) (void)");

            Add_Statement_In_Main
              (Rew.TU, Rew.Rewriter,
               "atexit (" & Dump_Procedure_Symbol (Main) & ");");

         when others =>
            null;
      end case;

   end Auto_Dump_Buffers_In_Main;

   overriding procedure Auto_Dump_Buffers_In_Main
     (Self     : C_Instrumenter_Type;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info)
   is
      Rew : C_Source_Rewriter;
   begin
      Rew.Start_Rewriting (Info, Filename);
      Auto_Dump_Buffers_In_Main (IC, Info, Main, Rew);
      Rew.Apply;
   end Auto_Dump_Buffers_In_Main;

   ----------------
   -- Format_Def --
   ----------------

   function Format_Def
     (C_Type     : String;
      Name       : String;
      Array_Size : String := "";
      Func_Args  : String := "";
      Init_Expr  : String := "";
      External   : Boolean := False) return String
   is
      Result : Unbounded_String;
   begin
      if External then
         Append (Result, "extern ");
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
     (File       : in out Text_Files.File_Type;
      C_Type     : String;
      Name       : String;
      Array_Size : String := "";
      Init_Expr  : String := "") is
   begin
      File.Put_Line
        (Format_Def (C_Type, Name, Array_Size, Init_Expr => Init_Expr));
   end Put_Format_Def;

   ------------------------
   -- Format_Extern_Decl --
   ------------------------

   function Format_Extern_Decl
     (C_Type    : String;
      Name      : String;
      Func_Args : String := "") return String is
   begin
      return
        Format_Def (C_Type, Name, Func_Args => Func_Args, External => True);
   end Format_Extern_Decl;

   ---------------------
   -- Put_Extern_Decl --
   ---------------------

   procedure Put_Extern_Decl
     (Rewriter  : Rewriter_T;
      Location  : Source_Location_T;
      C_Type    : String;
      Name      : String;
      Func_Args : String := "") is
   begin
      CX_Rewriter_Insert_Text_Before
        (Rew    => Rewriter,
         Loc    => Location,
         Insert => Format_Extern_Decl (C_Type, Name, Func_Args) & ASCII.LF);
   end Put_Extern_Decl;

   ---------------------
   -- Put_Extern_Decl --
   ---------------------

   procedure Put_Extern_Decl
     (File      : in out Text_Files.File_Type;
      C_Type    : String;
      Name      : String;
      Func_Args : String := "") is
   begin
      File.Put_Line (Format_Extern_Decl (C_Type, Name, Func_Args));
   end Put_Extern_Decl;

   --------------------------------
   -- Find_First_Insert_Location --
   --------------------------------

   function Find_First_Insert_Location
     (TU : Translation_Unit_T) return Source_Location_T
   is
      Location : Source_Location_T := Get_Null_Location;

      function Visit_Decl
        (Cursor : Cursor_T) return Child_Visit_Result_T with Convention => C;
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
                      Visitor => Visit_Decl'Unrestricted_Access);
      return Location;
   end Find_First_Insert_Location;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   overriding procedure Emit_Buffers_List_Unit
     (Self              : C_Instrumenter_Type;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info)
   is
      CU_Name_Body   : constant Compilation_Unit_Name :=
        CU_Name_For_File (+Buffers_List_Filename_Body (IC), IC.Project_Name);
      CU_Name_Header : constant Compilation_Unit_Name :=
        CU_Name_For_File (+Buffers_List_Filename_Header (IC), IC.Project_Name);

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
         Buffer_Unit_Length : constant String :=
           Count_Type'Image (Instr_Units.Length);
      begin
         --  Emit the body to contain the list of buffers

         Create_File
           (Root_Project_Info,
            File_Body,
            To_Filename
              (Root_Project_Info.Project,
               CU_Name_Body,
               C_Language));

         File_Body.Put_Line ("#include ""gnatcov_rts_c-buffers.h""");

         for Instr_Unit of Instr_Units loop
            Put_Extern_Decl
              (File_Body,
               "gnatcov_rts_unit_coverage_buffers",
               Unit_Buffers_Name (Instr_Unit));
         end loop;
         File_Body.Put_Line ("gnatcov_rts_unit_coverage_buffers_array "
                             & Unit_Buffers_Array_Name (IC) & " = {");
         File_Body.Put_Line ("  " & Buffer_Unit_Length & ",");
         File_Body.Put_Line ("  (gnatcov_rts_unit_coverage_buffers *[]) {");
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

      Create_File
        (Root_Project_Info,
         File_Header,
         To_Filename
           (Root_Project_Info.Project,
            CU_Name_Header,
            C_Language));

      Put_Extern_Decl
        (File_Header,
         "gnatcov_rts_unit_coverage_buffers_array",
         Unit_Buffers_Array_Name (IC));
   end Emit_Buffers_List_Unit;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   overriding procedure Instrument_Unit
     (Self      : C_Instrumenter_Type;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info)
   is
      Prj_Info : Project_Info renames Unit_Info.Prj_Info.all;
      UIC      : C_Unit_Inst_Context;
   begin
      Instrument_Source_File
        (CU_Name   => CU_Name,
         Unit_Info => Unit_Info,
         Prj_Info  => Prj_Info,
         IC        => IC,
         UIC       => UIC);

      --  Generate a buffer compilation unit defining coverage buffers that
      --  will store execution witnesses. This CU is a C file rather than an
      --  Ada file exporting the defined symboled to C. Indeed, we want it to
      --  be compatible with a C-only compiler.

      Emit_Buffer_Unit (Prj_Info, UIC);

      --  Track which CU_Id maps to which instrumented unit

      Instrumented_Unit_CUs.Insert (CU_Name, UIC.CU);
   end Instrument_Unit;

   ----------------------
   -- Skip_Source_File --
   ----------------------

   overriding function Skip_Source_File
     (Self        : C_Instrumenter_Type;
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
     (Args : in out String_Vectors.Vector; Options : Analysis_Options) is
   begin
      for Dir of Options.PP_Search_Path loop
         Args.Append (+"-I");
         Args.Append (Dir);
      end loop;

      for M of Options.PP_Macros loop
         declare
            Prefix : constant Unbounded_String :=
              +(if M.Define
                then "-D"
                else "-U");
         begin
            Args.Append (Prefix & M.Value);
         end;
      end loop;
   end Add_Options;

   -------------------------
   -- Import_From_Project --
   -------------------------

   procedure Import_From_Project
     (Self     : out Analysis_Options;
      Info     : Project_Info;
      Filename : String)
   is
      Switches : GNAT.Strings.String_List_Access;
   begin
      Self.PP_Macros :=
        Builtin_Macros (Compiler_Driver (Info.Project), +Info.Output_Dir).all;

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
              Index     => "c");
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
         Prefix : constant String := ('-', Option_Name);
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
     (Self : out Analysis_Options; Info : Project_Info; Filename : String) is
   begin
      Import_From_Project (Self, Info, Filename);
      for Args of Switches.Args.String_List_Args (Command_Line.Opt_C_Opts) loop
         Import_From_Args (Self, Split_Args (Args));
      end loop;
   end Import_Options;

end Instrument.C;
