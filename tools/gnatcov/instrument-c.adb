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

with Ada.Characters.Handling;
with Ada.Containers;  use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;     use Ada.Text_IO;

with Clang.CX_Diagnostic; use Clang.CX_Diagnostic;
with Clang.CX_String;     use Clang.CX_String;
with Clang.Extensions;    use Clang.Extensions;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

with GNATCOLL.Mmap;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with ALI_Files;           use ALI_Files;
with Coverage;            use Coverage;
with Coverage_Options;
with Hex_Images;          use Hex_Images;
with Outputs;             use Outputs;
with Paths;               use Paths;
with SCOs;
with System;              use System;
with Table;
with Text_Files;          use Text_Files;

package body Instrument.C is

   function To_Chars_Ptr_Array
     (V : String_Vectors.Vector) return chars_ptr_array;
   --  Convert a string vector to a chars_ptr_array. Result must be freed by
   --  the caller.

   procedure Free (Self : in out chars_ptr_array);
   --  Free all strings in Self

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

   procedure Get_Builtin_Macros
     (Lang, Compiler, Output_Dir : String;
      Compiler_Switches          : String_Vectors.Vector;
      Instr_Mode                 : Instrumentation_Mode;
      Result                     : out Macro_Set);
   --  Return the list of built-in macros for the given compiler, language and
   --  according to the compiler switches. Output_Dir is used to store a
   --  temporary file.

   procedure Preprocess_Source
     (Filename      : String;
      Instrumenter  : C_Family_Instrumenter_Type'Class;
      Prj           : Prj_Desc;
      PP_Filename   : out Unbounded_String;
      Options       : in out Analysis_Options;
      Keep_Comments : Boolean := False);
   --  Preprocess the source at Filename and extend Options using the Prj and
   --  the preprocessor output to retrieve standard search paths.
   --
   --  This uses the compiler in the Compiler_Driver project attribute to
   --  preprocess the file, assuming that it accepts the -E flag, to preprocess
   --  a file.

   function Common_Parse_TU_Args
     (Lang : Some_Language) return String_Vectors.Vector;
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
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name);
   --  Append a SCO to SCOs.SCO_Table. Also partially fill the preprocessing
   --  info: the actual source range referred, and the expanded macro name, if
   --  this is a SCO inside a macro expansion.

   type Instrument_Pass_Kind is new Pass_Kind with null record;

   overriding procedure Enter_Scope
     (Pass : Instrument_Pass_Kind;
      UIC  : in out C_Unit_Inst_Context'Class;
      N    : Cursor_T);
   --  Open a scope for N under the scope of the file in which the
   --  corresponding code was written. This must be completed with a call to
   --  the function Exit_Scope, defined below. Assume that the scope first SCO
   --  is the next generated SCO (SCOs.SCO_Table.Last + 1). Update
   --  UIC.Scopes to the created entity and UIC.Current_File_Scope to the
   --  corresponding file.

   overriding procedure Exit_Scope
     (Pass : Instrument_Pass_Kind;
      UIC  : in out C_Unit_Inst_Context'Class);
   --  Close the current scope, removing the current scope of the current file
   --  from UIC.Scopes if it does not contain SCOs. Assume that the last
   --  generated SCO (SCOs.SCO_Table.Last) is the last SCO for the current
   --  scope.

   procedure Remap_Scopes
     (Scopes  : in out Scopes_In_Files_Map.Map;
      SCO_Map : LL_HL_SCO_Map);
   --  Convert low level SCOs in each scope for each file to high-level SCOs
   --  using the mapping in SCO_Map. Set the file's SCO range to cover all of
   --  its scopes' SCO ranges.
   --
   --  Also remove from the Scopes map empty file scopes.

   overriding procedure Append_SCO
     (Pass               : Instrument_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
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
      State    : Unbounded_String);
   --  Add an entry to UIC.Source_Decisions

   overriding procedure Instrument_Condition
     (Pass      : Instrument_Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : Unbounded_String;
      First     : Boolean);
   --  Add an entry to UIC.Source_Conditions

   procedure Report
     (Pass : Instrument_Pass_Kind;
      Node : Cursor_T;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Warning);
   --  Report an instrumentation warning

   overriding procedure Insert_MCDC_State
     (Pass       : Instrument_Pass_Kind;
      UIC        : in out C_Unit_Inst_Context'Class;
      Name       : String;
      MCDC_State : out Unbounded_String);
   --  Wrapper around Insert_MCDC_State overload

   overriding procedure Insert_Text_Before_Token
     (Pass : Instrument_Pass_Kind;
      UIC  : C_Unit_Inst_Context'Class;
      Loc  : Source_Location_T;
      Text : String);

   overriding procedure Insert_Text_Before
     (Pass : Instrument_Pass_Kind;
      UIC  : C_Unit_Inst_Context'Class;
      Loc  : Source_Location_T;
      Text : String);

   overriding procedure Insert_Text_After
     (Pass : Instrument_Pass_Kind;
      UIC  : C_Unit_Inst_Context'Class;
      Loc  : Source_Location_T;
      Text : String);

   overriding procedure Register_CXX_For_Range
     (Pass : Instrument_Pass_Kind;
      UIC  : in out C_Unit_Inst_Context'Class;
      N    : Cursor_T);

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

   procedure Fix_CXX_For_Ranges (UIC : in out C_Unit_Inst_Context);
   --  This procedure fixes C++ for ranges that were purposefully instrumented
   --  wrongly. To instrument a C++ for range, we actually need to turn
   --
   --  for (int i = 0; auto j : {1, 2}) {}
   --
   --  into
   --
   --  {
   --     witness_i(); int i = 0;
   --     witness_j();
   --     for (auto j : {1, 2}) {}
   --  }
   --
   --  We introduce an outer scope to leave the initializer declaration
   --  lifetime unchanged, and to be able to easily instrument both the
   --  initializer declaration and the range expression.
   --
   --  Note that this is valid because you can't goto inside the loop
   --  (and thus skip the execution of witness_j) as it is
   --  forbidden to bypass declarations with initialization in C++,
   --  which "auto j : {1, 2}" is.
   --
   --  Though we can't do that all at once, as this changes the shape as the
   --  AST. As we rely on the AST node location to instrument statements and
   --  decisions, we would be instrumenting the decision at the wrong place,
   --  as we would instrument the initialization statement by moving it.
   --
   --  Thus, we do the normal instrumentation process, which will yield an
   --  unvalid C++ sequence, that we can easily fix in this procedure, by
   --  moving around the rewritten code.
   --
   --  The for ranges at the entry of this procedure will have been
   --  instrumented as followed (the added code is identified by <>):
   --
   --  <witness_j();> for (<witness_i();> int i = 0; auto j : {1, 2}) {}<}>
   --
   --  Two things to note here: the witness_j is executed before "int i = 0;"
   --  (which is wrong), and there is an unmatched closing brace.
   --
   --  To fix this, we actually need to move both the added code, _and_ the
   --  initializer statement before the <witness_j();>, and insert an opening
   --  brace before:
   --
   --  <{><witness_i();> int i = 0; <witness_j();> for (auto j : {1, 2}) {}<}>
   --
   --  and now we have valid, and correctly instrumented code.

   ----------------------------
   -- Source level rewriting --
   ----------------------------

   procedure Record_PP_Info
     (Orig_Filename : String;
      UIC           : in out C_Unit_Inst_Context;
      Instrumenter  : C_Family_Instrumenter_Type'Class);
   --  Emit the low-level SCOs for the given unit. Do not process them: this is
   --  left to the other (instrumentation) pass.

   ----------------------------
   -- Source instrumentation --
   ----------------------------

   procedure Emit_Buffer_Unit
     (UIC          : C_Unit_Inst_Context'Class;
      Unit         : Compilation_Unit;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj          : Prj_Desc);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit, for the given instrumenter.

   procedure Emit_Dump_Helper_Unit
     (Dump_Config       : Any_Dump_Config;
      Main              : Compilation_Unit_Part;
      Helper_Unit       : out Unbounded_String;
      Instrumenter      : C_Family_Instrumenter_Type'Class;
      Prj               : Prj_Desc);
   --  Emit the unit to contain helpers to implement the automatic dump of
   --  coverage buffers for the given Main unit. Info must be the project that
   --  owns this main. Upon return, the name of this helper unit is stored in
   --  Helper_Unit.

   procedure Check_Compiler_Driver
     (Prj          : Prj_Desc;
      Instrumenter : C_Family_Instrumenter_Type'Class);
   --  Check that a compiler driver exists for Instrumenter's language

   procedure Apply (Self : in out C_Source_Rewriter);
   --  Overwrite the file with the rewritter modifications

   procedure Start_Rewriting
     (Self         : out C_Source_Rewriter;
      Filename     : String;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj          : Prj_Desc);
   --  Start a rewriting session for the given file identified by its full
   --  name. If not already done, preprocess it beforehand.

   procedure Run_Diagnostics (TU : Translation_Unit_T);
   --  Output clang diagnostics on the given translation unit

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
     (Fingerprint : SC_Obligations.Fingerprint_Type) return String
   is (Instrument.Common.Format_Fingerprint (Fingerprint, "{", "}"));
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

   -----------------
   -- Enter_Scope --
   -----------------

   overriding procedure Enter_Scope
     (Pass : Instrument_Pass_Kind;
      UIC  : in out C_Unit_Inst_Context'Class;
      N    : Cursor_T)
   is
      procedure Enter_File_Scope
        (UIC : in out C_Unit_Inst_Context'Class;
         SFI : Source_File_Index)
      with Post => UIC.Scopes.Contains (SFI);
      --  Create a global scope for the file index SFI in which all scopes
      --  opened in the file will be stored.

      ----------------------
      -- Enter_File_Scope --
      ----------------------

      procedure Enter_File_Scope
        (UIC : in out C_Unit_Inst_Context'Class;
         SFI : Source_File_Index)
      is
         File_Scope_Position : Scopes_In_Files_Map.Cursor;

         Inserted : Boolean;
      begin
         if not UIC.Scopes.Contains (SFI) then

            --  Add a new entry to the file scopes map

            UIC.Scopes.Insert
              (SFI,
               File_Scope_Type'(others => <>),
               Position => File_Scope_Position,
               Inserted => Inserted);

            declare
               File_Scope    : constant Scopes_In_Files_Map.Reference_Type :=
                 UIC.Scopes.Reference (File_Scope_Position);
               New_Scope_Ent : constant Scope_Entity :=
                 (From       => SCO_Id (SCOs.SCO_Table.Last + 1),
                  To         => No_SCO_Id,
                  Name       => +Get_Simple_Name (SFI),
                  Sloc       => (Line => 0, Column => 0),
                  Identifier => (Decl_SFI => SFI, Decl_Line => 0));

               Scope_Entity_Position : Scope_Entities_Trees.Cursor;
            begin
               File_Scope.Scope_Entities.Insert_Child
                 (Parent   => File_Scope.Scope_Entities.Root,
                  Before   => Scope_Entities_Trees.No_Element,
                  New_Item => New_Scope_Ent,
                  Position => Scope_Entity_Position);
               File_Scope.Current_Scope_Entity := Scope_Entity_Position;
               File_Scope.File_Scope_Entity := Scope_Entity_Position;
            end;
         else
            File_Scope_Position := UIC.Scopes.Find (SFI);
         end if;

         UIC.Current_File_Scope := File_Scope_Position;
      end Enter_File_Scope;

      Sloc : constant Source_Location   := Start_Sloc (N);
      File : constant Source_File_Index := Sloc.Source_File;
   begin
      --  For C and C++ programs that can include header files we need to be
      --  able to know which file a scope was opened in. For this we use a map
      --  from Source_File_Index to Scope_Entity_Acc. The scope associated to a
      --  file is the last scope that was opened in it but not yet exited.
      --  This mechanism works in the same way as that for Ada with the
      --  possibility to have one current scope per file rather than just one
      --  only for the file being instrumented.

      Enter_File_Scope (UIC, File);

      declare
         C             : constant Scopes_In_Files_Map.Cursor :=
           UIC.Scopes.Find (File);
         File_Scope    : constant Scopes_In_Files_Map.Reference_Type :=
           UIC.Scopes.Reference (C);
         New_Scope_Ent : constant Scope_Entity :=
           (From       => SCO_Id (SCOs.SCO_Table.Last + 1),
            To         => No_SCO_Id,
            Name       => +Get_Decl_Name_Str (N),
            Sloc       => Sloc.L,
            Identifier => (Decl_SFI => File, Decl_Line => Sloc.L.Line));
         Inserted      : Scope_Entities_Trees.Cursor;
      begin
         --  Add New_Scope_Ent to the children of the last open scope in the
         --  file.

         File_Scope.Scope_Entities.Insert_Child
           (Parent   => File_Scope.Current_Scope_Entity,
            Before   => Scope_Entities_Trees.No_Element,
            New_Item => New_Scope_Ent,
            Position => Inserted);

         --  Make the current scope entity point to the right one

         File_Scope.Current_Scope_Entity := Inserted;
      end;
   end Enter_Scope;

   ----------------
   -- Exit_Scope --
   ----------------

   overriding procedure Exit_Scope
     (Pass : Instrument_Pass_Kind;
      UIC  : in out C_Unit_Inst_Context'Class)
   is
      File_Scope_Ref   : constant Scopes_In_Files_Map.Reference_Type :=
        UIC.Scopes.Reference (UIC.Current_File_Scope);
      Scope_Entity_Ref : constant Scope_Entities_Trees.Reference_Type :=
        File_Scope_Ref.Scope_Entities.Reference
          (File_Scope_Ref.Current_Scope_Entity);
      Parent           : constant Scope_Entities_Trees.Cursor :=
        Scope_Entities_Trees.Parent (File_Scope_Ref.Current_Scope_Entity);
   begin
      Scope_Entity_Ref.To := SCO_Id (SCOs.SCO_Table.Last);

      --  Discard the scope entity if it has no associated SCOs

      if Scope_Entity_Ref.To < Scope_Entity_Ref.From then
         File_Scope_Ref.Scope_Entities.Delete_Leaf
           (File_Scope_Ref.Current_Scope_Entity);
      else
         --  Update the last SCO for the file scope

         File_Scope_Ref.Scope_Entities.Reference
           (File_Scope_Ref.File_Scope_Entity).To := Scope_Entity_Ref.To;
      end if;

      --  Go back to the parent scope entity

      File_Scope_Ref.Current_Scope_Entity := Parent;
   end Exit_Scope;

   ------------------
   -- Remap_Scopes --
   ------------------

   procedure Remap_Scopes
     (Scopes  : in out Scopes_In_Files_Map.Map;
      SCO_Map : LL_HL_SCO_Map)
   is
      Res : Scopes_In_Files_Map.Map;
   begin
      for Cur in Scopes.Iterate loop
         declare
            File_Scope        : File_Scope_Type :=
              Scopes_In_Files_Map.Element (Cur);
            File_Scope_Entity : Scope_Entity renames
              Scope_Entities_Trees.Element (File_Scope.File_Scope_Entity);
         begin
            --  If the file scope is empty, do not add it to the resulting map

            if File_Scope_Entity.To >= File_Scope_Entity.From then
               Remap_Scope_Entities (File_Scope.Scope_Entities, SCO_Map);
               Res.Insert (Scopes_In_Files_Map.Key (Cur), File_Scope);
            end if;
         end;
      end loop;
      Scopes := Res;
   end Remap_Scopes;

   ----------------
   -- Append_SCO --
   ----------------

   overriding procedure Append_SCO
     (Pass               : Record_PP_Info_Pass_Kind;
      UIC                : in out C_Unit_Inst_Context'Class;
      N                  : Cursor_T;
      C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
      Loc  : Source_Location_T := Start_Sloc (N);
      Info : PP_Info;
   begin
      Append_SCO
        (C1, C2, From.L, To.L, From.Source_File, Last, Pragma_Aspect_Name);

      --  We add preprocessing information only for actual SCOs. Return there
      --  if this is an operator SCO.

      if C1 in '!' | '&' | '|' then
         return;
      end if;

      --  TODO??? The actual source range should be a Source_Location, and not
      --  a Local_Source_Location as it can refer to #included files.

      Info.Actual_Source_Range := (From.L, To.L);
      Info.PP_Source_Range := No_Local_Range;

      --  Check if this is comes from a macro expansion, in which case we need
      --  to record some information, for reporting purposes.

      if Is_Macro_Location (Loc) then
         declare
            Expansion_Stack : Expansion_Lists.List;
            Definition_Info : Expansion_Info;

            Macro_Expansion_Name    : Unbounded_String;
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
            --
            --  Another important point is that macro definitions can be
            --  given on the command line, which means that source locations
            --  that refer to the expansion of such macros will not have an
            --  associated file. This information is preserved through
            --  preprocessing, and we can actually use the Presumed_Location
            --  API to get either a filename, or a <command line> or <built-in>
            --  string in these cases.

            if Is_Macro_Arg_Expansion (Loc, Macro_Arg_Expanded_Loc, UIC.TU)
            then
               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics
                    (Macro_Arg_Expanded_Loc, UIC.TU);

               Definition_Info :=
                 (Macro_Name => Macro_Expansion_Name,
                  Sloc       =>
                    Presumed_Spelling_Location
                      (UIC.TU,
                       Macro_Arg_Expanded_Loc,
                       Macro_Expansion_Name,
                       UIC.Options.Builtin_Macros));
            else
               Macro_Expansion_Name :=
                 +Get_Immediate_Macro_Name_For_Diagnostics (Loc, UIC.TU);
               Definition_Info :=
                 (Macro_Name => Macro_Expansion_Name,
                  Sloc       =>
                    Presumed_Spelling_Location
                      (UIC.TU,
                       Loc,
                       Macro_Expansion_Name,
                       UIC.Options.Builtin_Macros));
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

               if Macro_Expansion_Name /= "" then
                  Expansion_Stack.Append
                    ((Macro_Name => Macro_Expansion_Name,
                      Sloc       =>
                        Presumed_Spelling_Location
                          (UIC.TU,
                           Immediate_Expansion_Loc,
                           Macro_Expansion_Name,
                           UIC.Options.Builtin_Macros)));
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
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
   begin
      --  Insert a new entry to the UIC.Instrumented_Entities maps: even if
      --  this SOI does not possess instrumented SCOs (it can be the case if
      --  all of the code constructs are not instrumentable), we want an empty
      --  Instrumented_Entities entry rather than no entry at all, to have
      --  proper initialization of checkpoints structures (e.g. Statement_Bits)
      --  later on.

      if not UIC.Instrumented_Entities.Contains (From.Source_File) then
         UIC.Instrumented_Entities.Insert (From.Source_File, (others => <>));
      end if;

      Append_SCO
        (C1, C2, From.L, To.L, From.Source_File, Last, Pragma_Aspect_Name);

      --  If this SCO is in a macro expansion, let's add source location
      --  information: we want to be able to know the actual source location
      --  of the SCO in the preprocessed code. This will allow us to
      --  retrieve the actual string (from the preprocessed code) when
      --  producing a coverage report.

      if UIC.LL_PP_Info_Map.Contains (SCOs.SCO_Table.Last) then
         declare
            Start_Loc : constant Source_Location_T := Start_Sloc (N);
            End_Loc   : constant Source_Location_T := End_Sloc (N);

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

   --------------------------------
   -- Find_Instrumented_Entities --
   --------------------------------

   function Find_Instrumented_Entities
     (UIC : in out C_Unit_Inst_Context'Class;
      SFI : Valid_Source_File_Index)
      return C_Instrumented_Entities_Maps.Reference_Type
   is
      Dummy : Boolean;
   begin
      return UIC.Instrumented_Entities.Reference (SFI);
   end Find_Instrumented_Entities;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   overriding procedure Insert_MCDC_State
     (Pass       : Instrument_Pass_Kind;
      UIC        : in out C_Unit_Inst_Context'Class;
      Name       : String;
      MCDC_State : out Unbounded_String) is
   begin
      if not UIC.Disable_Instrumentation then
         MCDC_State := +Insert_MCDC_State (UIC, Name);
      end if;
   end Insert_MCDC_State;

   ------------------------------
   -- Insert_Text_Before_Token --
   ------------------------------

   overriding procedure Insert_Text_Before_Token
     (Pass : Instrument_Pass_Kind;
      UIC  : C_Unit_Inst_Context'Class;
      Loc  : Source_Location_T;
      Text : String) is
   begin
      if not UIC.Disable_Instrumentation then
         CX_Rewriter_Insert_Text_Before_Token (UIC.Rewriter, Loc, Text);
      end if;
   end Insert_Text_Before_Token;

   ------------------------
   -- Insert_Text_Before --
   ------------------------

   overriding procedure Insert_Text_Before
     (Pass : Instrument_Pass_Kind;
      UIC  : C_Unit_Inst_Context'Class;
      Loc  : Source_Location_T;
      Text : String) is
   begin
      if not UIC.Disable_Instrumentation then
         CX_Rewriter_Insert_Text_Before (UIC.Rewriter, Loc, Text);
      end if;
   end Insert_Text_Before;

   -----------------------
   -- Insert_Text_After --
   -----------------------

   overriding procedure Insert_Text_After
     (Pass : Instrument_Pass_Kind;
      UIC  : C_Unit_Inst_Context'Class;
      Loc  : Source_Location_T;
      Text : String) is
   begin
      if not UIC.Disable_Instrumentation then
         CX_Rewriter_Insert_Text_After (UIC.Rewriter, Loc, Text);
      end if;
   end Insert_Text_After;

   ----------------------------
   -- Register_CXX_For_Range --
   ----------------------------

   overriding procedure Register_CXX_For_Range
     (Pass : Instrument_Pass_Kind;
      UIC  : in out C_Unit_Inst_Context'Class;
      N    : Cursor_T) is
   begin
      UIC.Instrumented_CXX_For_Ranges.Append (N);
   end Register_CXX_For_Range;

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
      if UIC.Disable_Instrumentation then
         UIC.Non_Instr_LL_SCOs.Include (SCO_Id (LL_SCO));
      else
         UIC.Find_Instrumented_Entities (Last_File).Statements.Append
           (C_Source_Statement'
              (LL_SCO       => LL_SCO,
               Instr_Scheme => Instr_Scheme,
               Statement    => Insertion_N));
      end if;
   end Instrument_Statement;

   -------------------------
   -- Instrument_Decision --
   -------------------------

   overriding procedure Instrument_Decision
     (Pass     : Instrument_Pass_Kind;
      UIC      : in out C_Unit_Inst_Context'Class;
      LL_SCO   : Nat;
      Decision : Cursor_T;
      State    : Unbounded_String) is
   begin
      if UIC.Disable_Instrumentation then
         UIC.Non_Instr_LL_SCOs.Include (SCO_Id (LL_SCO));
      else
         UIC.Find_Instrumented_Entities (Last_File).Decisions.Append
           (C_Source_Decision'
              (LL_SCO   => LL_SCO,
               Decision => Decision,
               State    => State));
      end if;
   end Instrument_Decision;

   --------------------------
   -- Instrument_Condition --
   --------------------------

   overriding procedure Instrument_Condition
     (Pass      : Instrument_Pass_Kind;
      UIC       : in out C_Unit_Inst_Context'Class;
      LL_SCO    : Nat;
      Condition : Cursor_T;
      State     : Unbounded_String;
      First     : Boolean) is
   begin
      if UIC.Disable_Instrumentation then
         UIC.Non_Instr_LL_SCOs.Include (SCO_Id (LL_SCO));
      else
         UIC.Find_Instrumented_Entities (Last_File).Conditions.Append
           (C_Source_Condition'
              (LL_SCO    => LL_SCO,
               Condition => Condition,
               State     => State,
               First     => First));
      end if;
   end Instrument_Condition;

   ------------
   -- Report --
   ------------

   procedure Report
     (Pass : Instrument_Pass_Kind;
      Node : Cursor_T;
      Msg  : String;
      Kind : Report_Kind := Diagnostics.Warning)
   is
      Sloc : constant Source_Location := Start_Sloc (Node);
   begin
      Diagnostics.Report ((Source_File => Sloc.Source_File,
                           L           => (Line   => Sloc.L.Line,
                                           Column => Sloc.L.Column)),
                          Msg,
                          Kind);
   end Report;

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

      if SC.State = "" then
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
              & (+SC.State) & ", "
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
           Start_Sloc (N),
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
                       & (+SD.State),
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

   ------------------------
   -- Fix_CXX_For_Ranges --
   ------------------------

   procedure Fix_CXX_For_Ranges (UIC : in out C_Unit_Inst_Context) is
   begin
      for N of UIC.Instrumented_CXX_For_Ranges loop
         declare
            Loc           : constant Source_Location_T := Start_Sloc (N);
            For_Init_Stmt : constant Cursor_T := Get_For_Init (N);
            For_Init_Rng  : constant Source_Range_T :=
              Get_Cursor_Extent (For_Init_Stmt);
         begin
            --  Get the rewritten text for the initializing statement and
            --  move it before any rewritten text before the for statement.

            CX_Rewriter_Insert_Text_Before
              (UIC.Rewriter,
               Loc,
               CX_Rewriter_Get_Rewritten_Text (UIC.Rewriter, For_Init_Rng));

            --  Open the outer scope. It was closed during the instrumentation
            --  process, so we do not need to take care of that.

            CX_Rewriter_Insert_Text_Before (UIC.Rewriter, Loc, "{");
            CX_Rewriter_Remove_Text (UIC.Rewriter, For_Init_Rng);

            --  for (; auto i : {1, 2}) is invalid C++ code so insert a dummy
            --  initializing expression to avoid the null statement here, as
            --  it is easier than trying to move around the comma.

            CX_Rewriter_Insert_Text_Before
              (UIC.Rewriter, Start_Sloc (For_Init_Stmt), "true");
         end;
      end loop;
   end Fix_CXX_For_Ranges;

   -------------------------
   -- Traverse_Statements --
   -------------------------

   procedure Traverse_Statements
     (UIC             : in out C_Unit_Inst_Context;
      L               : Cursor_Vectors.Vector;
      Trailing_Braces : out Unbounded_String);
   --  Process L, a list of statements or declarations. Set Trailing_Braces
   --  to the list of braces that should be inserted after this statements'
   --  list.

   procedure Traverse_Declarations
     (UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector);
   --  Traverse a translation unit (top level declarations)

   procedure Process_Expression
     (UIC : in out C_Unit_Inst_Context;
      N   : Cursor_T;
      T   : Character);
   --  Process the decisions, and the lambda expressions in N

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

   ------------------------
   -- Process_Expression --
   ------------------------

   procedure Process_Expression
     (UIC : in out C_Unit_Inst_Context;
      N   : Cursor_T;
      T   : Character)
   is
      function Process_Lambda_Expr
        (Cursor : Cursor_T) return Child_Visit_Result_T;
      --  Helper for Visit_Children. Process every lambda expr under Cursor,
      --  _but_ the lambda expressions nested in other lambda expressions.

      procedure Process_Decisions
        (UIC : in out C_Unit_Inst_Context;
         N   : Cursor_T;
         T   : Character);

      -------------------------
      -- Process_Lambda_Expr --
      -------------------------

      function Process_Lambda_Expr
        (Cursor : Cursor_T) return Child_Visit_Result_T is
      begin
         if Kind (Cursor) = Cursor_Lambda_Expr then
            Traverse_Declarations
              (UIC => UIC,
               L   => Cursor_Vectors.To_Vector (Cursor, 1));
            return Child_Visit_Continue;
         end if;
         return Child_Visit_Recurse;
      end Process_Lambda_Expr;

      -----------------------
      -- Process_Decisions --
      -----------------------

      procedure Process_Decisions
        (UIC : in out C_Unit_Inst_Context;
         N   : Cursor_T;
         T   : Character)
      is
         Mark : Nat;
         --  This is used to mark the location of a decision sequence in the
         --  SCO table. We use it for backing out a simple decision in an
         --  expression context that contains only NOT operators.

         Mark_Hash : Nat;
         --  Likewise for the putative SCO_Raw_Hash_Table entries: see below

         type Hash_Entry is record
            Sloc      : Source_Location;
            SCO_Index : Nat;
         end record;
         --  We must register all conditions/pragmas in SCO_Raw_Hash_Table.
         --  However we cannot register them at the same time we are adding the
         --  corresponding SCO entries to the raw table since we may discard
         --  them later on. So instead we put all putative conditions into
         --  Hash_Entries (see below) and register them once we are sure we
         --  keep them.
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
         --  Hold temporarily (i.e. free'd before returning) the Hash_Entry
         --  before they are registered in SCO_Raw_Hash_Table.

         ---------------------------------
         -- Decision-specific variables --
         ---------------------------------

         --  The following variables are related to the current decision being
         --  processed by this call to Process_Decisions. Note that in the case
         --  of nested decisions, this subprogram recurses, so we do not have
         --  to worry about overwriting them.

         Current_Decision : Nat;
         --  Low level SCO id of current decision

         X_Not_Decision : Boolean;
         --  This flag keeps track of whether a decision sequence in the
         --  SCO table contains only NOT operators, and is for an expression
         --  context (T=X). The flag will be set False if T is other than X,
         --  or if an operator other than NOT is in the sequence.

         Condition_Count : Natural := 0;
         --  Count of conditions for current decision (MC/DC only)

         MCDC_State : Unbounded_String;
         --  Name of MC/DC state local variable for current decision (MC/DC
         --  only).

         procedure Output_Decision_Operand (Operand : Cursor_T);
         --  The node Operand is the top level logical operator of a decision,
         --  or it is one of the operands of a logical operator belonging to
         --  a single complex decision. This (recursive) routine outputs the
         --  sequence of table entries corresponding to the node. Note that we
         --  do not process the sub- operands to look for further decisions,
         --  that processing is done in Find_Nested_Decisions, because we can't
         --  get decisions mixed up in the global table. Call has no effect
         --  if Operand is Empty. Increments Condition_Count (recursively)
         --  for each condition.

         procedure Output_Element (N : Cursor_T);
         --  Node N is an operand of a logical operator that is not itself a
         --  logical operator, or it is a simple decision. This routine outputs
         --  the table entry for the element, with C1 set to ' '. Last is set
         --  False, and an entry is made in the condition hash table.

         procedure Output_Header (T : Character; N : Cursor_T);
         --  Outputs a decision header node. T is I/W/E/P for IF/WHILE/EXIT
         --  WHEN/ PRAGMA, and 'X' for the expression case. Resets
         --  Condition_Count to 0, and initializes MCDC_State.

         procedure Find_Nested_Decisions (Operand : Cursor_T);
         --  This is called on node Operand, the top level node of a decision,
         --  or on one of its operands or suboperands after generating the full
         --  output for the complex decision. It process the suboperands of the
         --  decision looking for nested decisions.

         function Process_Node (N : Cursor_T) return Child_Visit_Result_T;
         --  Processes one node in the traversal, looking for logical
         --  operators, and if one is found, outputs the appropriate
         --  table entries.

         -----------------------------
         -- Output_Decision_Operand --
         -----------------------------

         procedure Output_Decision_Operand (Operand : Cursor_T) is
            C1 : Character;
            C2 : Character;
            --  C1 holds a character that identifies the operation while C2
            --  indicates whether we are sure (' ') or not ('?') this operation
            --  belongs to the decision. '?' entries will be filtered out in
            --  the second (SCO_Record_Filtered) pass.

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
                  To   => Slocs.No_Location,
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
                    (UIC,
                     Make_MCDC_State_Name (SCOs.SCO_Table.Last),
                     MCDC_State);
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
            --  Test for the two cases where N is the root node of some
            --  decision:

            Decision_Root : constant Boolean :=

               --  Simple decision at outer level: a boolean expression (which
               --  is not a logical operator) appearing as the operand of an
               --  IF, WHILE, FOR construct.

              (N = Process_Decisions.N and then T /= 'X')
              or else

               --  Complex decision, whether at outer level or nested: a
               --  boolean expression involving a logical operator.

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

                  --  If the decision was in an expression context (T =
                  --  'X') and contained only NOT operators, then we do not
                  --  output it, so delete the associated SCO entries. As a
                  --  consequence, no instrumentation will be emitted.

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
            when Cursor_Lambda_Expr =>

               --  Do not descend into lambdas, the decisions inside the lambda
               --  will be taken care of while processing the statements in the
               --  lambda.

               return Child_Visit_Continue;
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

   begin
      Process_Decisions (UIC, N, T);
      Visit (N, Process_Lambda_Expr'Access);
   end Process_Expression;

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
                  Outputs.Warning_Or_Error
                    ("Error when parsing the file " & Str);
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
     (UIC             : in out C_Unit_Inst_Context;
      L               : Cursor_Vectors.Vector;
      Trailing_Braces : out Unbounded_String)
   is
      procedure Traverse_One
        (N : Cursor_T; Trailing_Braces : out Unbounded_String);
      --  Traverse a statement. Set Trailing_Braces to the list of braces that
      --  should be inserted after this statement.

      procedure Instrument_Statement
        (N            : Cursor_T;
         Typ          : Character;
         Insertion_N  : Cursor_T := Get_Null_Cursor;
         Instr_Scheme : Instr_Scheme_Type := Instr_Stmt);
      --  Instrument the statement N.
      --
      --  Typ is the letter that identifies the type of statement/declaration
      --  that is being instrumented.
      --
      --  N is the original node from user code, and controls the source
      --  location assigned to the statement SCO.
      --
      --  In general, this is also where the witness statement is inserted, but
      --  in some rare cases, it needs to be inserted at a different place,
      --  or in a different manner: this is configured by the Insertion_N and
      --  Instr_Scheme formals.

      function Curlify (N : Cursor_T) return Boolean;
      --  Rewrite control flow statements (if else, for, while etc.) with
      --  braces when needed (i.e. when they do not already have them). Note
      --  that the insertion of the last closing brace is deferred to the
      --  traversal of the following statement, and thus, Curlify returns
      --  True if the insertion of a closing brace is required.
      --
      --  We can't insert this closing brace with N source location information
      --  only, as the ending location of a statement expression is the end
      --  location of the expression, and not the location of the semicolon
      --  terminating the statement. To solve this, we choose to insert the
      --  closing brace _before_ the next statement (or _before the closing
      --  brace of the function body, if this is the last statement of the
      --  function) rather than _after_ the statement semicolon, which can't
      --  be done easily.

      ------------------
      -- Traverse_One --
      ------------------

      procedure Traverse_One
        (N : Cursor_T; Trailing_Braces : out Unbounded_String) is
         use Cursor_Vectors;
         TB : Unbounded_String;
         --  Local to pass on to Traverse_Statements invocations. Trailing
         --  braces that can be inserted while instrumenting the current node
         --  (e.g. for the then part of an if-else statement, the closing
         --  brace will be inserted before the else location) will be inserted.
         --  For the other cases, the insertion of the closing braces will
         --  be delayed to the instrumentation of the next statement (see
         --  Traverse_Statements body) or, if this is the last statement, after
         --  being done with the instrumentation of the function (see the body
         --  of Traverse_Declarations).

         Save_Disable_Instrumentation : constant Boolean :=
           UIC.Disable_Instrumentation;
      begin
         if Curlify (N) then
            Append (Trailing_Braces, '}');
         end if;

         --  If this statement does not belong to a source of interest, skip
         --  it altogether.

         if Is_Null (N) or not Is_Source_Of_Interest (UIC, N) then
            return;
         end if;

         case Kind (N) is

            when Cursor_Compound_Stmt =>
               Traverse_Statements (UIC, Get_Children (N), TB);

            when Cursor_If_Stmt =>
               Instrument_Statement (N, 'I');

               declare
                  Then_Part : constant Cursor_T := Get_Then (N);
                  Else_Part : constant Cursor_T := Get_Else (N);
               begin
                  if Is_Constexpr (N) then
                     UIC.Pass.Report
                       (N,
                        "gnatcov limitation: cannot instrument constexpr if"
                        & " statement.");
                     UIC.Disable_Instrumentation := True;
                  end if;
                  Process_Expression (UIC, Get_Cond (N), 'I');
                  UIC.Disable_Instrumentation := Save_Disable_Instrumentation;
                  Traverse_Statements (UIC, To_Vector (Then_Part), TB);

                  --  Traverse the ELSE statements if present

                  if not Is_Null (Else_Part) then

                     --  Insert the trailing braces resulting from the
                     --  traversal of the then part before the else.

                     UIC.Pass.Insert_Text_Before (UIC, Get_Else_Loc (N), +TB);
                     TB := +"";
                     Traverse_Statements (UIC, To_Vector (Else_Part), TB);
                  end if;
               end;

            when Cursor_Switch_Stmt =>
               Instrument_Statement (N, 'C');
               declare
                  Switch_Cond : constant Cursor_T := Get_Cond (N);
                  Alt         : constant Cursor_T := Get_Body (N);
               begin
                  Process_Expression (UIC, Switch_Cond, 'X');

                  --  Process case branches

                  Traverse_Statements (UIC, To_Vector (Alt), TB);
               end;

            --  Case alternative

            when Cursor_Case_Stmt | Cursor_Default_Stmt =>
               declare
                  Case_Body : constant Cursor_T := Get_Sub_Stmt (N);
               begin
                  Traverse_Statements (UIC, To_Vector (Case_Body), TB);
               end;

            when Cursor_While_Stmt =>
               declare
                  While_Body : constant Cursor_T := Get_Body (N);
                  Cond_Var   : constant Cursor_T := Get_Cond_Var (N);
                  Cond       : constant Cursor_T := Get_Cond (N);

               begin
                  --  If the loop condition is a declaration, instrument its
                  --  initialization expression.

                  Instrument_Statement
                    (N, 'W',
                     Insertion_N  => (if Is_Null (Cond_Var)
                                      then Cond
                                      else Get_Var_Init_Expr (Cond_Var)),
                     Instr_Scheme => Instr_Expr);

                  Process_Expression (UIC, Cond, 'W');
                  Traverse_Statements (UIC, To_Vector (While_Body), TB);
               end;

            when Cursor_Do_Stmt =>
               declare
                  Do_Body  : constant Cursor_T := Get_Body (N);
                  Do_While : constant Cursor_T := Get_Cond (N);

               begin
                  Traverse_Statements (UIC, To_Vector (Do_Body), TB);

                  --  Insert the trailing braces resulting from the body
                  --  traversal before the while.

                  UIC.Pass.Insert_Text_After (UIC, Get_While_Loc (N), +TB);
                  TB := +"";

                  --  Process the while decision

                  Instrument_Statement
                    (Do_While, 'W', Instr_Scheme => Instr_Expr);
                  Process_Expression (UIC, Do_While, 'W');

               end;

            when Cursor_For_Stmt =>
               declare
                  For_Init : constant Cursor_T := Get_For_Init (N);
                  For_Cond : constant Cursor_T := Get_Cond (N);
                  For_Inc  : constant Cursor_T := Get_For_Inc (N);
                  For_Body : constant Cursor_T := Get_Body (N);
               begin
                  Instrument_Statement
                    (For_Init, ' ', Insertion_N => N);
                  Instrument_Statement
                    (For_Cond, 'F', Instr_Scheme => Instr_Expr);

                  --  The guard expression for the FOR loop is a decision. The
                  --  closest match for this kind of decision is a while loop.

                  Process_Expression (UIC, For_Cond, 'W');

                  Traverse_Statements (UIC, To_Vector (For_Body), TB);

                  Instrument_Statement
                    (For_Inc, ' ', Instr_Scheme => Instr_Expr);
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

                  if not Is_Null (For_Init_Stmt) then

                     --  See Fix_CXX_For_Ranges for an explanation of the
                     --  below code.

                     Instrument_Statement
                       (For_Init_Stmt, ' ', Insertion_N  => For_Init_Stmt);
                     Process_Expression (UIC, For_Init_Stmt, 'X');

                     --  Preemptively end the introduced outer scope as it is
                     --  easier done when traversing the AST.

                     Append (Trailing_Braces, '}');
                     UIC.Pass.Register_CXX_For_Range (UIC, N);
                  end if;

                  --  Instrument the range as mentioned above

                  Instrument_Statement
                    (For_Range_Decl, ' ',
                     Insertion_N  => N,
                     Instr_Scheme => Instr_Stmt);
                  Process_Expression (UIC, For_Range_Decl, 'X');

                  --  Generate obligations for body statements

                  Traverse_Statements (UIC, To_Vector (For_Body), TB);
               end;

            when Cursor_Goto_Stmt | Cursor_Indirect_Goto_Stmt =>
               Instrument_Statement (N, ' ');

            when Cursor_Label_Stmt =>
               Traverse_Statements (UIC, Get_Children (N), TB);

            when Cursor_Stmt_Expr =>
               Traverse_Statements (UIC, Get_Children (N), TB);

            --  Null statement, we won't monitor their execution

            when Cursor_Null_Stmt =>
               null;

            --  TODO??? There are probably missing special statements, such as
            --  ternary operator etc. Do that in a later step.

            when Cursor_Call_Expr =>
                  --  Check the name of the callee. If the callee is the manual
                  --  dump buffers procedure, do nothing. It should not be
                  --  considered for coverage analysis.

               if not Is_Manual_Dump_Procedure_Symbol (Get_Callee_Name_Str (N))
               then
                  Instrument_Statement (N, ' ');
                  if Has_Decision (N) then
                     Process_Expression (UIC, N, 'X');
                  end if;
               end if;

            when others =>

               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               Instrument_Statement (N, ' ');

               --  Process any embedded decisions

               if Is_Constexpr (N) then
                  UIC.Pass.Report
                    (N,
                     "gnatcov limitation: cannot instrument constexpr"
                     & " variable declarations.");
                  UIC.Disable_Instrumentation := True;
                  Process_Expression (UIC, N, 'X');
                  UIC.Disable_Instrumentation :=
                    Save_Disable_Instrumentation;
               else
                  Process_Expression (UIC, N, 'X');
               end if;
         end case;

         Append (Trailing_Braces, TB);
      end Traverse_One;

      --------------------------
      -- Instrument_Statement --
      --------------------------

      procedure Instrument_Statement
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
         if Is_Null (N) then
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

         UIC.Pass.Append_SCO
           (UIC  => UIC,
            N    => N,
            C1   => 'S',
            C2   => Typ,
            From => F,
            To   => T,
            Last => True);

         UIC.Pass.Instrument_Statement
           (UIC          => UIC,
            LL_SCO       => SCOs.SCO_Table.Last,
            Insertion_N  => (if Is_Null (Insertion_N) then N else Insertion_N),
            Instr_Scheme => Instr_Scheme);
      end Instrument_Statement;

      -------------
      -- Curlify --
      -------------

      function Curlify (N : Cursor_T) return Boolean is
      begin
         case Kind (N) is
            when Cursor_If_Stmt =>
               declare
                  Then_Part : constant Cursor_T := Get_Then (N);
                  Else_Part : constant Cursor_T := Get_Else (N);
               begin
                  if Kind (Then_Part) /= Cursor_Compound_Stmt then
                     UIC.Pass.Insert_Text_Before
                       (UIC, Start_Sloc (Then_Part), "{");
                     if not Is_Null (Else_Part) then
                        --  Close the brace introduced to wrap the then part
                        --  if possible.

                        UIC.Pass.Insert_Text_Before
                          (UIC, Get_Else_Loc (N), "}");
                     else
                        --  Otherwise, we need to insert a trailing brace

                        return True;
                     end if;
                  end if;

                  --  Then, deal with the else if it is not a compound stmt

                  if not Is_Null (Else_Part)
                    and then Kind (Else_Part) /= Cursor_Compound_Stmt
                  then
                     UIC.Pass.Insert_Text_Before
                       (UIC, Start_Sloc (Else_Part), "{");
                     return True;
                  end if;
                  return False;
               end;

            when Cursor_Do_Stmt =>
               declare
                  Do_Body : constant Cursor_T := Get_Body (N);
               begin
                  if Kind (Do_Body) /= Cursor_Compound_Stmt then
                     UIC.Pass.Insert_Text_Before
                       (UIC, Start_Sloc (Do_Body), "{");
                     UIC.Pass.Insert_Text_Before
                       (UIC, Get_While_Loc (N), "}");
                  end if;
                  return False;
               end;
            when Cursor_While_Stmt
               | Cursor_For_Stmt
               | Cursor_CXX_For_Range_Stmt =>
               declare
                  B : constant Cursor_T := Get_Body (N);
               begin
                  if Kind (B) /= Cursor_Compound_Stmt then
                     UIC.Pass.Insert_Text_Before (UIC, Start_Sloc (B), "{");
                     return True;
                  end if;
                  return False;
               end;
            when others =>
               return False;
         end case;
      end Curlify;

      use Cursor_Vectors;

   --  Start of processing for Traverse_Statements

   begin
      for N of L loop
         if Trailing_Braces /= "" then
            UIC.Pass.Insert_Text_Before
              (UIC, Start_Sloc (N), +Trailing_Braces);
            Trailing_Braces := +"";
         end if;
         Traverse_One (N, Trailing_Braces);
      end loop;
   end Traverse_Statements;

   ---------------------------
   -- Traverse_Declarations --
   ---------------------------

   procedure Traverse_Declarations
     (UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector)
   is
      use Cursor_Vectors;
      Saved_MCDC_State_Declaration_Node : constant Cursor_T :=
        UIC.MCDC_State_Declaration_Node;
      Save_Disable_Instrumentation      : constant Boolean :=
        UIC.Disable_Instrumentation;
   begin
      for N of L loop

         --  Only traverse the function declarations that belong to a unit of
         --  interest.
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
                     Fun_Body : constant Cursor_T := Get_Body (N);
                     Stmts    : constant Cursor_Vectors.Vector :=
                       Get_Children (Fun_Body);
                     --  Get_Body returns a Compound_Stmt, convert it to a list
                     --  of statements using the Get_Children utility.

                     TB : Unbounded_String;
                     --  Trailing braces that should be inserted at the end
                     --  of the function body.

                  begin
                     UIC.Pass.Enter_Scope (UIC, N);

                     --  Do not instrument constexpr function as it would
                     --  violate the constexpr restrictions.

                     if Is_Constexpr (N) then
                        UIC.Pass.Report
                          (N,
                           "gnatcov limitation: cannot instrument constexpr"
                           & " functions.");
                        UIC.Disable_Instrumentation := True;
                     end if;

                     if Stmts.Length > 0 then
                        UIC.MCDC_State_Declaration_Node :=
                          Stmts.First_Element;
                        Traverse_Statements (UIC, Stmts, TB);
                        UIC.Pass.Insert_Text_Before_Token
                          (UIC, End_Sloc (Fun_Body), +TB);
                     end if;
                     UIC.Pass.Exit_Scope (UIC);
                     UIC.Disable_Instrumentation :=
                       Save_Disable_Instrumentation;
                  end;

               --  Traverse the declarations of a namespace / linkage
               --  specification etc.

               when Cursor_Namespace
                  | Cursor_Class_Template
                  | Cursor_Class_Decl
                  | Cursor_Struct_Decl =>

                  UIC.Pass.Enter_Scope (UIC, N);

                  Traverse_Declarations (UIC, Get_Children (N));

                  UIC.Pass.Exit_Scope (UIC);

               when Cursor_Linkage_Spec =>
                  Traverse_Declarations (UIC, Get_Children (N));

               when others =>
                  null;
            end case;

         end if;
      end loop;

      --  Restore previous MCDC_State insertion node: we can have lambda
      --  expressions inside functions, and we don't want to keep inserting
      --  MCDC state variables in the lambda after we finished its traversal.

      UIC.MCDC_State_Declaration_Node := Saved_MCDC_State_Declaration_Node;
   end Traverse_Declarations;

   --------------------
   -- Builtin_Macros --
   --------------------

   procedure Get_Builtin_Macros
     (Lang, Compiler, Output_Dir : String;
      Compiler_Switches          : String_Vectors.Vector;
      Instr_Mode                 : Instrumentation_Mode;
      Result                     : out Macro_Set)
   is
      use Ada.Characters.Handling;

      PID : constant Unsigned_64 :=
        Unsigned_64 (Pid_To_Integer (Current_Process_Id));

      L        : constant String := To_Lower (Lang);
      Args     : String_Vectors.Vector;
      Basename : constant String :=
        Ada.Directories.Simple_Name (Compiler)
        & "_builtins_"
        & Hex_Image (PID);
      Filename : constant String := Output_Dir / Basename;
      File     : Ada.Text_IO.File_Type;
   begin
      --  Run the preprocessor on an empty file and write the
      --  preprocessed sources to Filename.

      --  If we are in the integrated instrumentation mode, we use the compiler
      --  (as opposed to the compiler driver) to preprocess the source file.
      --  The compiler - which is specific to the language - does not accept
      --  the -x switch, so do not pass it in this case.

      if Instr_Mode = Project_Instrumentation then
         Args.Append (+"-x");
         Args.Append (+L);
      else
         --  Add the -quiet switch to prevent the compiler from outputting
         --  execution time statistics. TODO??? Check whether this is compiler
         --  specific (cc1 / cc1plus) when implementing integrated
         --  instrumentation support for other compilers.

         Args.Append (+"-quiet");
      end if;

      Args.Append (Compiler_Switches);
      Args.Append (+"-E");
      Args.Append (+"-dM");
      Args.Append (+"-");

      Run_Command
        (Command             => Compiler,
         Arguments           => Args,
         Origin_Command_Name => "gnatcov instrument",
         Output_File         => Filename,
         In_To_Null          => True);

      --  Decode all macro definitions in Filename and store them in Result

      Open (File, In_File, Filename);
      while not End_Of_File (File) loop
         declare
            Line      : constant String := Get_Line (File);
            Macro_Def : Macro_Definition (Define => True);
            Success   : Boolean;
         begin
            Parse_Macro_Definition
              (Line,
               Macro_Def,
               Success);
            if Success then
               Result.Include (Macro_Def);
            else
               Warn
                 ("Cannot parse a built-in macro definition for "
                  & Compiler & ", ignoring it:" & ASCII.LF & "  "
                  & Line);
            end if;
         end;
      end loop;
      Close (File);
      Delete_File (Filename);
   end Get_Builtin_Macros;

   -----------------------
   -- Preprocess_Source --
   -----------------------

   procedure Preprocess_Source
     (Filename      : String;
      Instrumenter  : C_Family_Instrumenter_Type'Class;
      Prj           : Prj_Desc;
      PP_Filename   : out Unbounded_String;
      Options       : in out Analysis_Options;
      Keep_Comments : Boolean := False)
   is
      Cmd : Command_Type;
      --  The command to preprocess the file

      Success : Boolean;
      --  Whether this command is successful

      PID : constant Unsigned_64 :=
        Unsigned_64 (Pid_To_Integer (Current_Process_Id));

      Preprocessed_Filename : constant String :=
        (+Prj.Output_Dir) / ("pp-" & Strip_Zero_Padding (Hex_Image (PID)));
      --  Preprocessed file. We then postprocess it to remove redundant line
      --  markers inserted by the preprocessor.

      Preprocessor_Output_Filename : constant String :=
        (+Prj.Output_Dir) /
        ("pp-output-" & Strip_Zero_Padding (Hex_Image (PID)));
      Preprocessor_Output_File     : Ada.Text_IO.File_Type;
      --  File containing the preprocessor output (used to get include search
      --  paths).
   begin
      Import_Options (Options, Instrumenter, Prj, Filename);
      PP_Filename := +New_File (Prj, Filename);

      --  If the preprocessed output file already exists, consider that it was
      --  already preprocessed and do nothing.

      if Ada.Directories.Exists (+PP_Filename) then
         return;
      end if;

      Cmd :=
        (Command => Prj.Compiler_Driver (Instrumenter.Language),
         others  => <>);

      if Keep_Comments then
         Append_Arg (Cmd, "-C");
      end if;

      --  Add the preprocessing flag

      Append_Arg (Cmd, "-E");

      --  Add all of the compiler / compiler driver options to the
      --  preprocessing command line.

      Append_Args (Cmd, Options.Raw_Switches);

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
      Append_Arg (Cmd, Preprocessed_Filename);

      --  Run the preprocessing command, keep track of whether it was
      --  successful for later

      Success := Run_Command
        (Command             => Cmd,
         Origin_Command_Name => "Preprocessing",
         Output_File         => Preprocessor_Output_Filename,
         Ignore_Error        => True);

      --  Clear the search path so that we populate it from the include search
      --  paths in the logs.

      Options.PP_Search_Path.Clear;

      --  Retrieve the include search paths. They are delimited by:
      --  #include "..." search starts here:
      --  #include <...> search starts here:
      --  ...
      --  End of search list

      Open (Preprocessor_Output_File, In_File, Preprocessor_Output_Filename);

      declare
         RE_Begin_Pattern      : constant Pattern_Matcher :=
           Compile ("#include .* search starts here");
         Begin_Pattern_Matched : Boolean := False;
         RE_End_Pattern        : constant Pattern_Matcher :=
           Compile ("End of search list");
         Matches               : Match_Array (0 .. 0);
      begin
         while not End_Of_File (Preprocessor_Output_File) loop
            declare
               Line : constant String := Get_Line (Preprocessor_Output_File);
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
               Reset (Preprocessor_Output_File);
            end if;

            while not End_Of_File (Preprocessor_Output_File) loop
               Put_Line (Get_Line (Preprocessor_Output_File));
            end loop;
            Delete (Preprocessor_Output_File);
            Fatal_Error ("Preprocessing failed: aborting");
         end if;
         Delete (Preprocessor_Output_File);
      end;

      --  Now, onto the postprocessing. Remove spurious system header line
      --  markers.

      Postprocess_Source (Preprocessed_Filename, +PP_Filename);
      Ada.Directories.Delete_File (Preprocessed_Filename);
   end Preprocess_Source;

   --------------------------
   -- Common_Parse_TU_Args --
   --------------------------

   function Common_Parse_TU_Args
     (Lang : Some_Language) return String_Vectors.Vector
   is
      Command_Line_Args : String_Vectors.Vector;
      use Ada.Characters.Handling;
      use String_Vectors;
   begin
      --  We will get errors when parsing a gcc-preprocessed file with clang:
      --  portions of the standard library may refer to compiler builtins.
      --  To work around clang incompletely parsing the file in that case,
      --  unset the error limit.

      Append (Command_Line_Args, +"-ferror-limit=0");

      --  Pass explicitly the language through the command-line, as we can
      --  redefine file suffixes with gprbuild, and <file>.c can be a C++
      --  file.

      Append (Command_Line_Args, +"-x");
      Append (Command_Line_Args, +To_Lower (Image (Lang)));

      --  As we also pass compiler warning switches, as they can influence the
      --  preprocessing through the use of the __has_warning macro, we make
      --  sure to disable warnings on unknown warnings (as some warnings are
      --  e.g. gcc-specific and won't be recognized by clang).

      Append (Command_Line_Args, +"-Wno-unknown-warning-option");

      return Command_Line_Args;
   end Common_Parse_TU_Args;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self         : out C_Source_Rewriter;
      Filename     : String;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj          : Prj_Desc)
   is
      PP_Filename : Unbounded_String;

      Options : Analysis_Options;
      Args    : String_Vectors.Vector;
   begin
      Preprocess_Source
        (Filename,
         Instrumenter,
         Prj,
         PP_Filename,
         Options);

      Self.CIdx :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      --  At this point, whether Preprocessed was passed True or False to
      --  Start_Rewriting, the code to parse is always preprocessed.

      Add_Options (Args, Options, Preprocessed => True);
      String_Vectors.Append
        (Args, Common_Parse_TU_Args (Instrumenter.Language));

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
         if Misc_Trace.Is_Active then
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

   --------------------
   -- Record_PP_Info --
   --------------------

   procedure Record_PP_Info
     (Orig_Filename : String;
      UIC           : in out C_Unit_Inst_Context;
      Instrumenter  : C_Family_Instrumenter_Type'Class)
   is
      Args : String_Vectors.Vector;
   begin
      UIC.Pass := Record_PP_Info_Pass'Access;
      UIC.CIdx :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      --  Get the predefined macros and search paths of the user's compiler and
      --  inhibit the use of clang predefined macros. We want to fully emulate
      --  the user's preprocessor.

      Add_Options (Args, UIC.Options);
      String_Vectors.Append
        (Args, Common_Parse_TU_Args (Instrumenter.Language));

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
        (UIC => UIC,
         L   => Get_Children (Get_Translation_Unit_Cursor (UIC.TU)));

   end Record_PP_Info;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit
     (Self              : in out C_Family_Instrumenter_Type;
      Unit_Name         : String;
      Prj               : Prj_Desc;
      Files_Of_Interest : File_Sets.Set)
   is
      UIC     : C_Unit_Inst_Context;
      CU_Name : constant Compilation_Unit_Part :=
        CU_Name_For_File (+Unit_Name);

      Orig_Filename : constant String  := Unit_Name;
      PP_Filename   : Unbounded_String;
      --  Respectively original, and preprocessed filename

      Buffer_Filename : constant String :=
        +Self.Buffer_Unit
           (Compilation_Unit'(File_Based_Language, +Unit_Name), Prj).Unit_Name;
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
            Self,
            C_Type,
            Name,
            Array_Size,
            Func_Args);
      end Put_Extern_Decl;

   --  Start of processing for Instrument_Source_File

   begin
      --  Exit early if there is no compiler driver found to preprocess the
      --  source.

      Check_Compiler_Driver (Prj, Self);

      SCOs.Initialize;

      UIC.SFI := Get_Index_From_Generic_Name
        (Orig_Filename,
         Kind                => Files_Table.Source_File,
         Indexed_Simple_Name => True);
      UIC.Fullname := +Orig_Filename;

      --  Initialize the C instrumentation context

      UIC.Instrumented_Unit := CU_Name;
      UIC.Buffer_Unit := CU_Name_For_File (+Buffer_Filename);
      UIC.Files_Of_Interest := Files_Of_Interest;

      --  Run the preprocessor (this also takes care of importing the
      --  preprocessor options into UIC.Options).

      Preprocess_Source (Orig_Filename, Self, Prj, PP_Filename, UIC.Options);

      --  Start by recording preprocessing information

      Record_PP_Info (Orig_Filename, UIC, Self);

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
                       ((Sloc (Get_Token_Location (UIC.TU, Token)), Ann));
                  end if;
                  Match (End_Matcher, Comment, Match_Res);
                  if Match_Res (0) /= No_Match then
                     Ann.Kind := Exempt_Off;
                     UIC.Annotations.Append
                       ((Sloc
                           (Get_Range_End (Get_Token_Extent (UIC.TU, Token))),
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

      --  Save the last SCO of the first pass (for a consistency check with
      --  the second pass later), and reset the SCO tables for the
      --  instrumentation pass.

      Record_PP_Info_Last_SCO := SCOs.SCO_Table.Last;
      SCOs.Initialize;

      --  Then, instrument

      UIC.Pass := Instrument_Pass'Access;

      Start_Rewriting
        (Self         => Rewriter,
         Filename     => +PP_Filename,
         Instrumenter => Self,
         Prj          => Prj);
      UIC.TU := Rewriter.TU;
      UIC.Rewriter := Rewriter.Rewriter;
      Insert_Extern_Location :=
        Start_Sloc (Get_Translation_Unit_Cursor (UIC.TU));

      Traverse_Declarations
        (UIC => UIC,
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
         Import_Non_Instrumented_LL_SCOs (UIC, SCO_Map);

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
               --  Allocate bits in coverage buffers and insert the
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
                           if Path_Count (Decision) > Get_Path_Count_Limit
                           then
                              Set_Decision_SCO_Non_Instr_For_MCDC (Decision);
                           else
                              Insert_Condition_Witness
                                (UIC, SC, Offset_For_True (Condition));
                           end if;
                        end;
                     end loop;
                  end if;
               end if;

               --  Create mappings from allocated bits to the corresponding SCO
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

         --  Iterate through the package level body entities

         Remap_Scopes (UIC.Scopes, SCO_Map);

         for C in UIC.Scopes.Iterate loop
            declare
               CU : constant Created_Unit_Maps.Cursor :=
                 UIC.CUs.Find (Scopes_In_Files_Map.Key (C));
            begin
               if Created_Unit_Maps.Has_Element (CU) then
                  Set_Scope_Entities
                    (Created_Unit_Maps.Element (CU),
                     Scopes_In_Files_Map.Element (C).Scope_Entities);
               end if;
            end;
         end loop;
      end;

      --  Once everything has been instrumented, fixup the for ranges. See the
      --  documentation of Fix_CXX_For_Ranges.

      Fix_CXX_For_Ranges (UIC);

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

      Rewriter.Apply;

      --  Generate a buffer compilation unit defining coverage buffers that
      --  will store execution witnesses. This CU is a C file rather than an
      --  Ada file exporting the defined symboled to C. Indeed, we want it to
      --  be compatible with a C-only compiler.

      Emit_Buffer_Unit
        (UIC,
         Compilation_Unit'
           (Language     => File_Based_Language,
            Unit_Name    => UIC.Fullname),
         Self,
         Prj);

      --  Update the Ignore_Status of the CU we instrumented

      Files_Table.Consolidate_Ignore_Status
        (Index  => Files_Table.Get_Index_From_Generic_Name
           (Name                => Orig_Filename,
            Kind                => Files_Table.Source_File,
            Indexed_Simple_Name => True),
         Status => Files_Table.Never);
   end Instrument_Unit;

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
         Append (Result, (+Exprs.Element (I)));
      end loop;
      if Multiline then
         Append (Result, ASCII.LF);
      end if;
      Append (Result, "}");
      return (+Result);
   end Format_Array_Init_Expr;

   ----------------------
   -- Emit_Buffer_Unit --
   ----------------------

   procedure Emit_Buffer_Unit
     (UIC          : C_Unit_Inst_Context'Class;
      Unit         : Compilation_Unit;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj          : Prj_Desc)
   is
      CU_Name : Compilation_Unit_Part renames UIC.Buffer_Unit;
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
      --  Compute Buffers_CUs and Buffers_CU_Names

      for Cur in UIC.Sources_Of_Interest_Info.Iterate loop
         declare
            use Created_Unit_Maps;

            SOI   : Source_Of_Interest renames
              UIC.Sources_Of_Interest_Info.Constant_Reference (Cur);
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

      Create_File (Prj, File, +CU_Name.Filename);

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

            CU          : constant CU_Id := Buffers_CUs (Buffers_Index);
            CU_Name     : Compilation_Unit_Part renames
              Buffers_CU_Names.Constant_Reference (Buffers_Index);
            CU_Filename : constant String := +CU_Name.Filename;

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
               --
               --  We also have to be careful to escape the backslashes inside
               --  the string, which can appear e.g. in in windows full
               --  filenames.

               & "  .unit_name = "
               & " {" & C_String_Literal (CU_Filename) & ", "
               & CU_Filename'Length'Image & "}"
               & ","
               & ASCII.LF

               & "  .bit_maps_fingerprint = "
               & Format_Fingerprint (SC_Obligations.Bit_Maps_Fingerprint (CU))
               & ","
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
            Unit_Buffers_Name (Unit),
            Init_Expr =>
              "{" & Img (Buffers_Count) & ", &" & Buffers_Array & "[0]}");
      end;
   end Emit_Buffer_Unit;

   ---------------------------
   -- Emit_Dump_Helper_Unit --
   ---------------------------

   procedure Emit_Dump_Helper_Unit
     (Dump_Config       : Any_Dump_Config;
      Main              : Compilation_Unit_Part;
      Helper_Unit       : out Unbounded_String;
      Instrumenter      : C_Family_Instrumenter_Type'Class;
      Prj               : Prj_Desc)
   is
      File : Text_Files.File_Type;

      Output_Proc : constant String :=
        (case Dump_Config.Channel is
            when Binary_File => "gnatcov_rts_write_trace_file_wrapper",
            when Base64_Standard_Output =>
              "gnatcov_rts_write_trace_file_base64");

      Indent1 : constant String := "    ";
      Indent2 : constant String := Indent1 & "  ";

   begin
      --  Create the dump helper unit

      Helper_Unit := (if Dump_Config.Trigger = Manual
                      then
                         Instrumenter.Dump_Manual_Helper_Unit (Prj).Unit_Name
                      else Instrumenter.Dump_Helper_Unit
                        (Compilation_Unit'(File_Based_Language, Main.Filename),
                         Prj).Unit_Name);

      --  Compute the qualified names we need for instrumentation

      declare
         Filename       : constant String := +Helper_Unit;
         Dump_Procedure : constant String :=
           Dump_Procedure_Symbol
             (Main, Dump_Config.Trigger = Manual, Prj_Name => +Prj.Prj_Name);
      begin
         --  Emit the package body

         Create_File (Prj, File, Filename);

         File.Put_Line ("#include ""gnatcov_rts_c-strings.h""");

         case Dump_Config.Channel is
            when Binary_File =>
               File.Put_Line ("#include """
                              & "gnatcov_rts_c-traces-output-files.h""");
               File.Put_Line ("#include ""gnatcov_rts_c-os_interface.h""");
            when Base64_Standard_Output =>
               File.Put_Line ("#include """
                              & "gnatcov_rts_c-traces-output-base64.h""");
         end case;
         File.Put_Line ("#include <stdlib.h>");

         --  Import the coverage buffers

         Put_Extern_Decl
           (File,
            Instrumenter,
            "const struct gnatcov_rts_coverage_buffers_group_array",
            Unit_Buffers_Array_Name (+Prj.Prj_Name));

         --  Emit the procedure to write the trace file

         File.New_Line;
         File.Put (Instrumenter.Extern_Prefix);
         File.Put_Line ("void " & Dump_Procedure & " ("
                        & (if Dump_Config.Trigger = Manual
                           then "char *prefix"
                           else "void")
                        & ") {");

         File.Put_Line (Indent1 & Output_Proc & " (");
         File.Put_Line
           (Indent2 & "&"
            & Unit_Buffers_Array_Name (+Prj.Prj_Name) & ",");
         case Dump_Config.Channel is
         when Binary_File =>
            declare
               Env_Var : constant String :=
                 (if Dump_Config.Filename_Env_Var = ""
                  then "GNATCOV_RTS_DEFAULT_TRACE_FILENAME_ENV_VAR"
                  else """" & (+Dump_Config.Filename_Env_Var) & """");
               Prefix  : constant String :=
                 (if Dump_Config.Trigger = Manual
                  then "prefix"
                  else   """" & (+Dump_Config.Filename_Prefix) & """");
               Tag     : constant String := """" & (+Instrumenter.Tag) & """";
               Simple  : constant String :=
                 (if Dump_Config.Filename_Simple then "1" else "0");
            begin
               File.Put_Line
                 (Indent2 & "gnatcov_rts_default_trace_filename(");
               File.Put_Line (Indent2 & Env_Var & ",");
               File.Put_Line (Indent2 & Prefix & ",");
               File.Put_Line (Indent2 & Tag & ",");
               File.Put_Line (Indent2 & Simple & "),");

               File.Put_Line (Indent2
                              & "STR ("
                              & C_String_Literal
                                  (if Dump_Config.Trigger = Manual
                                   then +Prj.Prj_Name
                                   else +Main.Filename)
                              & "),");
               File.Put_Line (Indent2 & "gnatcov_rts_time_to_uint64()" & ",");
               File.Put_Line (Indent2 & "STR ("""")");
            end;

         when Base64_Standard_Output =>

            --  Configurations using this channel generally run on embedded
            --  targets and have a small runtime, so our best guess for the
            --  program name is the name of the main, and there is no way to
            --  get the current execution time.

            File.Put_Line (Indent2 & "STR ("
                           & C_String_Literal
                               (if Dump_Config.Trigger = Manual
                                then +Prj.Prj_Name
                                else +Main.Filename)
                           & "),");
            File.Put_Line (Indent2 & "0,");
            File.Put_Line (Indent2 & "STR ("""")");

         end case;
         File.Put_Line (Indent1 & ");");

         File.Put_Line ("}");

         File.Close;
      end;
   end Emit_Dump_Helper_Unit;

   ----------------------------------
   -- Emit_Dump_Helper_Unit_Manual --
   ----------------------------------

   overriding procedure Emit_Dump_Helper_Unit_Manual
     (Self        : in out C_Family_Instrumenter_Type;
      Dump_Config : Any_Dump_Config;
      Prj         : Prj_Desc)
   is
      Main : Compilation_Unit_Part;
      --  Since the dump trigger is "manual" and there is no main to be given,
      --  the Main argument in the following call to Emit_Dump_Helper_Unit will
      --  not be used.

      Dummy_Unit_Name : Unbounded_String;

   begin
      Emit_Dump_Helper_Unit
        (Dump_Config  => Dump_Config,
         Main         => Main,
         Helper_Unit  => Dummy_Unit_Name,
         Instrumenter => Self,
         Prj          => Prj);
   end Emit_Dump_Helper_Unit_Manual;

   ------------------------------------
   -- Replace_Manual_Dump_Indication --
   ------------------------------------

   overriding procedure Replace_Manual_Dump_Indication
     (Self                  : in out C_Family_Instrumenter_Type;
      Prj                   : in out Prj_Desc;
      Source                : GNATCOLL.Projects.File_Info;
      Has_Manual_Indication : out Boolean)
   is
      Orig_Filename : constant String := +Source.File.Full_Name;
   begin
      Check_Compiler_Driver (Prj, Self);

      declare
         Options        : Analysis_Options;
         PP_Filename    : Unbounded_String;
         Dummy_Main     : Compilation_Unit_Part;
         Dump_Pat       : constant Pattern_Matcher :=
           Compile
             ("^[\t ]*\/\* GNATCOV_DUMP_BUFFERS (\((.*)\))? \*\/[ \t]*",
              Flags => Multiple_Lines);
         Matches        : Match_Array (0 .. 2);
         Dump_Procedure : constant String :=
           Dump_Procedure_Symbol
             (Main => Dummy_Main, Manual => True, Prj_Name => +Prj.Prj_Name);
      begin
         --  Preprocess the source, keeping the comment to look for the manual
         --  dump indication later.

         Preprocess_Source
           (Orig_Filename, Self, Prj, PP_Filename, Options, True);
         declare
            use Files_Handling.File_To_String_Vectors_Maps;
            Cur      : Cursor;
            Inserted : Boolean;
         begin
            --  We need to save the search paths that were used to preprocess
            --  the file, as they must be passed on to clang parsing
            --  invocations (especially when we are parsing the original file
            --  to record preprocessing information).

            for Path of Options.PP_Search_Path loop
               Prj.Compiler_Options_Unit.Insert
                 (Key       => Source.File,
                  New_Item  => String_Vectors.Empty_Vector,
                  Position  => Cur,
                  Inserted  => Inserted);
               Prj.Compiler_Options_Unit.Reference (Cur).Append ("-I" & Path);
            end loop;
         end;

         --  Look for the manual dump indication in the preprocessed file. Use
         --  the GNATCOLL.Mmap API to map the file contents in memory, as we
         --  may need to rewrite it to the source file, with the manual dump
         --  indication replaced by an actual call to the dump buffers
         --  function.

         declare
            use Ada.Streams.Stream_IO;
            use GNATCOLL.Mmap;
            File         : Mapped_File := Open_Read (To_String (PP_Filename));
            Region       : Mapped_Region := Read (File);
            Raw_Str      : constant Str_Access := Data (Region);
            Raw_Str_Last : constant Natural := Last (Region);
            Str          : String renames Raw_Str (1 .. Raw_Str_Last);

            Tmp_Filename : constant String := +PP_Filename & ".tmp";

            Output_File : Ada.Streams.Stream_IO.File_Type;
            S           : Ada.Streams.Stream_IO.Stream_Access;
            --  Temporary file containing the new version of the original file,
            --  with inserted calls to dump buffers. The original file is then
            --  overwritten by this temporary file. This file is opened as a
            --  binary file, as we are going to write contents retrieved
            --  through mmap (thus binary content) to it.

            Index : Positive := 1;
            --  Starting index, or last index of the previous match in the
            --  original file.

         begin
            Has_Manual_Indication := False;
            while Index in Str'Range loop
               Match (Dump_Pat, Str (Index .. Str'Last), Matches);
               exit when Matches (0) = No_Match;

               --  Open the output file if this is the first match we find,
               --  then forward the source code that appear before the match
               --  unchanged.

               if not Has_Manual_Indication then
                  Create (Output_File, Out_File, Tmp_Filename);
                  S := Stream (Output_File);
                  Has_Manual_Indication := True;

                  --  Put an external decl for the buffers dump function

                  String'Write
                    (S, "extern void " & Dump_Procedure & "(char *prefix);");
               end if;
               String'Write (S, Str (Index .. Matches (0).First));

               --  Replace the match with the call to the dump procedure.
               --  Use the project name as prefix if none is given by the user.

               if Matches (2) = No_Match then
                  String'Write
                    (S, Dump_Procedure & "(" & (+Prj.Prj_Name) & ");");
               else
                  String'Write
                    (S, Dump_Procedure & "("
                        & Str (Matches (2).First .. Matches (2).Last) & ");");
               end if;
               Index := Matches (0).Last + 1;
            end loop;

            --  If we had a manual indication, and thus wrote a modified source
            --  file, overwrite the original source file with it.

            if Has_Manual_Indication then
               declare
                  PP_File  : constant Virtual_File := Create (+(+PP_Filename));
                  Tmp_File : constant Virtual_File := Create (+Tmp_Filename);
                  Success  : Boolean;
               begin
                  --  Flush the rest of the file contents

                  String'Write (S, Str (Index .. Str'Last));
                  Close (Output_File);

                  Free (Region);
                  Close (File);

                  --  Overwrite the original file with its newer version

                  PP_File.Delete (Success);
                  Tmp_File.Rename
                    (Full_Name => PP_File,
                     Success   => Success);
                  if not Success then
                     Outputs.Fatal_Error
                       ("Failed to replace manual dump indication for Source "
                        & (+Source.File.Full_Name));
                  end if;
               end;
            else
               Free (Region);
               Close (File);
            end if;
         end;
      end;
   end Replace_Manual_Dump_Indication;

   -------------------------------
   -- Auto_Dump_Buffers_In_Main --
   -------------------------------

   overriding procedure Auto_Dump_Buffers_In_Main
     (Self          : in out C_Family_Instrumenter_Type;
      Filename      : String;
      Dump_Config   : Any_Dump_Config;
      Prj           : Prj_Desc)
   is
      Helper_Filename : Unbounded_String;
      --  Name of file to contain helpers implementing the buffers dump

      Rew  : C_Source_Rewriter;
      Main : constant Compilation_Unit_Part :=
        (Language_Kind => File_Based_Language,
         Filename      => +Ada.Directories.Full_Name (Filename));

      Insert_Extern_Location : Source_Location_T;
      --  Where to insert extern declarations

      Main_Cursor : Cursor_T;
      --  Cursor of the main declaration
   begin
      Check_Compiler_Driver (Prj, Self);

      Rew.Start_Rewriting (Filename, Self, Prj);

      Insert_Extern_Location :=
        Start_Sloc (Get_Translation_Unit_Cursor (Rew.TU));
      Main_Cursor := Get_Main (Rew.TU);
      if Main_Cursor = Get_Null_Cursor then
         Outputs.Fatal_Error
           ("Could not find main function in "
            & (Ada.Directories.Simple_Name (+Main.Filename)));
      end if;

      Emit_Dump_Helper_Unit
        (Dump_Config, Main, Helper_Filename, Self, Prj);

      Put_Extern_Decl
        (Rew.Rewriter,
         Insert_Extern_Location,
         Self,
         "void",
         Dump_Procedure_Symbol (Main),
         Func_Args => "void");

      if Dump_Config.Trigger = Ravenscar_Task_Termination then
         Warn ("--dump-trigger=ravenscar-task-termination is not valid for a C"
               & " main. Defaulting to --dump-trigger=main-end for this"
               & " main.");
      end if;

      case Dump_Config.Trigger is
         when Main_End | Ravenscar_Task_Termination =>
            declare
               use Cursor_Vectors;

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
                        --  Replace "return <expr>;" with
                        --  "return <tmp>=<expr>, <dump_buffers>(), <tmp>;"
                        --
                        --  Note that a <tmp> variable declaration is
                        --  introduced at the beginning of the main.

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

               declare
                  Main_Body  : constant Cursor_T := Get_Body (Main_Cursor);
                  Main_Stmts : constant Vector := Get_Children (Main_Body);
               begin
                  --  Introduce a variable to hold the return value. Declare
                  --  it at the start of the main to avoid complications with
                  --  curly braces and control flow constructs.

                  if Length (Main_Stmts) /= 0 then
                     declare
                        Main_First_Stmt : constant Cursor_T :=
                          Main_Stmts.First_Element;
                     begin
                        Insert_Text_Before_Start_Of
                          (N    => Main_First_Stmt,
                           Text => "int gnatcov_rts_return;",
                           Rew  => Rew.Rewriter);
                     end;
                  end if;

                  Visit (Main_Body, Process'Access);

                  --  If the main function does not end with a return, add
                  --  a call to dump_buffers at the end of the function.

                  if Length (Main_Stmts) = 0
                       or else
                     Kind (Main_Stmts.Last_Element) /= Cursor_Return_Stmt
                  then
                     CX_Rewriter_Insert_Text_Before_Token
                       (Rew.Rewriter,
                        End_Sloc (Main_Body),
                        Dump_Procedure_Symbol (Main) & "();");
                  end if;
               end;
            end;

         when At_Exit =>

            --  To avoid getting conflicting atexit declarations (if the user
            --  includes stdlib.h), we only emit a declaration of atexit if
            --  is not already declared in the translation unit.

            if not Is_Atexit_Declared (Rew.TU) then
               Put_Extern_Decl
                 (Rew.Rewriter,
                  Insert_Extern_Location,
                  Self,
                  "int",
                  "atexit",
                  Func_Args => "void (*function) (void)");
            end if;

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
      Rew.Apply;
   end Auto_Dump_Buffers_In_Main;

   -----------------
   -- Buffer_Unit --
   -----------------

   function Buffer_Unit
     (Self : C_Family_Instrumenter_Type;
      CU   : Compilation_Unit;
      Prj  : Prj_Desc) return Compilation_Unit
   is
      Filename : constant String :=
        New_File
          (Prj,
           To_Symbol_Name (Sys_Prefix) & "_b_"
           & Filename_Slug (+CU.Unit_Name)
           & (+Prj.Body_Suffix
             (C_Family_Instrumenter_Type'Class (Self).Language)));
   begin
      return Compilation_Unit'
        (Language  => File_Based_Language,
         Unit_Name => +Filename);
   end Buffer_Unit;

   -----------------------------
   -- Dump_Manual_Helper_Unit --
   -----------------------------

   overriding function Dump_Manual_Helper_Unit
     (Self : C_Family_Instrumenter_Type;
      Prj  : Prj_Desc) return Compilation_Unit
   is
      Filename : constant String :=
        New_File
          (Prj,
           To_Symbol_Name (Sys_Prefix)
           & "_d_b_" & (+Prj.Prj_Name)
           & (+Prj.Body_Suffix
             (C_Family_Instrumenter_Type'Class (Self).Language)));
   begin
      return Compilation_Unit'
        (Language  => File_Based_Language,
         Unit_Name => +Filename);
   end Dump_Manual_Helper_Unit;

   ----------------------
   -- Dump_Helper_Unit --
   ----------------------

   overriding function Dump_Helper_Unit
     (Self : C_Family_Instrumenter_Type;
      CU   : Compilation_Unit;
      Prj  : Prj_Desc) return Compilation_Unit
   is
      Filename : constant String :=
        New_File
          (Prj,
           To_Symbol_Name (Sys_Prefix)
           & "_d_"
           & Filename_Slug (+CU.Unit_Name)
           & (+Prj.Body_Suffix
             (C_Family_Instrumenter_Type'Class (Self).Language)));
   begin
      return Compilation_Unit'
        (Language  => File_Based_Language,
         Unit_Name => +Filename);
   end Dump_Helper_Unit;

   --------------
   -- Has_Main --
   --------------

   overriding function Has_Main
     (Self     : in out C_Family_Instrumenter_Type;
      Filename : String;
      Prj      : Prj_Desc) return Boolean
   is
      Rew : C_Source_Rewriter;

      Main_Cursor : Cursor_T;
      --  Cursor of the main declaration

   begin
      Rew.Start_Rewriting (Filename, Self, Prj);
      Main_Cursor := Get_Main (Rew.TU);
      if Main_Cursor = Get_Null_Cursor then
         return False;
      else
         return True;
      end if;
   end Has_Main;

   ---------------------------
   -- Check_Compiler_Driver --
   ---------------------------

   procedure Check_Compiler_Driver
     (Prj          : Prj_Desc;
      Instrumenter : C_Family_Instrumenter_Type'Class)
   is
      Compiler_Driver : constant Unbounded_String :=
        Prj.Compiler_Driver (Instrumenter.Language);
   begin
      if Compiler_Driver = "" then
         Outputs.Fatal_Error
           ("could not find a compiler for " & Image (Instrumenter.Language));
      end if;
   end Check_Compiler_Driver;

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
      return +Result;
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

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   overriding procedure Emit_Buffers_List_Unit
     (Self        : C_Family_Instrumenter_Type;
      Instr_Units : Unit_Sets.Set;
      Prj         : Prj_Desc)
   is
      Buffer_Symbols : String_Sets.Set;
      Ignore_CU      : Compilation_Unit;
   begin
      for Instr_Unit of Instr_Units loop
         Buffer_Symbols.Insert (+Unit_Buffers_Name (Instr_Unit));
      end loop;
      Ignore_CU :=
        C_Family_Instrumenter_Type'Class (Self).Emit_Buffers_List_Unit
          (Buffer_Symbols => Buffer_Symbols, Prj => Prj);
   end Emit_Buffers_List_Unit;

   overriding function Emit_Buffers_List_Unit
     (Self           : C_Family_Instrumenter_Type;
      Buffer_Symbols : String_Sets.Set;
      Prj            : Prj_Desc) return Compilation_Unit
   is
      CU_Name : constant Compilation_Unit :=
        (Language => File_Based_Language,
         Unit_Name =>
           +New_File
             (Prj,
              "gcvrtc-" & (+Prj.Prj_Name)
              & (+Prj.Body_Suffix
                (C_Family_Instrumenter_Type'Class (Self).Language))));

      CU_File : Text_Files.File_Type;

      Buffer_Array_Decl  : constant String :=
        "const struct gnatcov_rts_coverage_buffers_group_array "
        & Unit_Buffers_Array_Name (+Prj.Prj_Name);
      Buffer_Unit_Length : constant String :=
        Count_Type'Image (Buffer_Symbols.Length);
   begin
      --  Emit the body to contain the list of buffers

      CU_File.Create (+CU_Name.Unit_Name);

      CU_File.Put_Line ("#include ""gnatcov_rts_c-buffers.h""");

      --  First create extern declarations for the buffers group of each unit

      for BS of Buffer_Symbols loop
         Put_Extern_Decl
           (CU_File,
            Self,
            "const struct gnatcov_rts_coverage_buffers_group",
            +BS);
      end loop;

      --  Then create an extern declaration for the buffer array (necessary in
      --  C++ to set the C linkage), and finally the definition for that array.

      CU_File.Put_Line (Self.Extern_Prefix & Buffer_Array_Decl & ";");
      CU_File.Put_Line (Buffer_Array_Decl & " = {");
      CU_File.Put_Line ("  " & Buffer_Unit_Length & ",");
      if String_Sets.Length (Buffer_Symbols) = 0 then

         --  If there are no units of interest, create a NULL pointer to avoid
         --  having an empty array.

         CU_File.Put_Line ("NULL");
      else
         CU_File.Put_Line
           ("  (const struct gnatcov_rts_coverage_buffers_group *[]) {");
         for BS of Buffer_Symbols loop
            CU_File.Put ("    &" & (+BS));
            if BS /= Buffer_Symbols.Last_Element then
               CU_File.Put_Line (",");
            end if;
         end loop;
         CU_File.Put_Line ("}");
      end if;
      CU_File.Put_Line ("};");
      return CU_Name;
   end Emit_Buffers_List_Unit;

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
     (Self         : out Analysis_Options;
      Instrumenter : C_Family_Instrumenter_Type'Class;
      Prj          : Prj_Desc;
      Filename     : String)
   is
      File         : constant Virtual_File := Create_Normalized (Filename);
      Cmdline_Opts : constant String_Vectors.Vector :=
        (case C_Family_Language (Instrumenter.Language) is
            when C_Language   => C_Opts,
            when CPP_Language => CPP_Opts);
      Prj_Options  : String_Vectors.Vector;
   begin
      --  Grab the options from the project description. Note that the project
      --  description is filled with compiler options in the -j1
      --  implementation. Otherwise, the compiler switches in the project files
      --  are passed through the command line directly.

      if Prj.Compiler_Options_Unit.Contains (File) then
         Prj_Options.Append (Prj.Compiler_Options_Unit.Element (File));
      else
         Prj_Options.Append (Prj.Compiler_Options (Instrumenter.Language));
      end if;
      Prj_Options.Append (Prj.Search_Paths);

      Import_From_Args (Self, Prj_Options);
      for Args of Cmdline_Opts loop
         Import_From_Args (Self, Split_Args (Args));
      end loop;

      --  Now, we can generate the preprocessor configuration (i.e. the set
      --  of predefined macros).

      Get_Builtin_Macros
        (Image (C_Family_Language (Instrumenter.Language)),
         +Prj.Compiler_Driver (Instrumenter.Language),
         +Prj.Output_Dir,
         Self.Compiler_Switches,
         Instrumenter.Instr_Mode,
         Self.Builtin_Macros);
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
      File   : Virtual_File;
   begin
      Get_Presumed_Location (Location => Loc,
                             Filename => C_File'Access,
                             Line     => Line'Access,
                             Column   => Column'Access);
      File := Create_Normalized (Get_C_String (C_File));

      --  Look for a corresponding entry in UIC.Sources_Of_Interest, create one
      --  if it is missing.

      declare
         use Source_Of_Interest_Maps;

         Cur : constant Cursor := UIC.Sources_Of_Interest_Info.Find (File);
         SOI : Source_Of_Interest;
      begin
         if Has_Element (Cur) then
            return UIC.Sources_Of_Interest_Info.Reference (Cur).Of_Interest;
         end if;

         --  Consider that File is of interest iff it belongs to a loaded
         --  project. TODO??? This should also consider units of interest
         --  switches.

         if UIC.Files_Of_Interest.Contains (File) then
            declare
               SFI : constant Source_File_Index :=
                 Get_Index_From_Generic_Name (+File.Full_Name, Source_File);
            begin
               SOI :=
                 (Of_Interest  => True,
                  SFI          => SFI,
                  CU_Name      => CU_Name_For_File (Full_Name (File)));
            end;
         else
            SOI := (Of_Interest => False);
         end if;
         UIC.Sources_Of_Interest_Info.Insert (File, SOI);

         return SOI.Of_Interest;
      end;
   end Is_Source_Of_Interest;

   ----------------------
   -- C_String_Literal --
   ----------------------

   function C_String_Literal (Str : String) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '"');
      for C of Str loop
         if C = '\' then
            Append (Result, "\\");
         elsif C = '"' then
            Append (Result, "\""");
         else
            Append (Result, C);
         end if;
      end loop;
      Append (Result, '"');
      return +Result;
   end C_String_Literal;

end Instrument.C;
