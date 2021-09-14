------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

with Ada.Characters;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers;             use Ada.Containers;
with Ada.Directories;            use Ada.Directories;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;

with Clang.Extensions; use Clang.Extensions;
with Clang.CX_String;  use Clang.CX_String;

with GNAT.Strings; use GNAT.Strings;
with GNAT.String_Split;
with GNAT.OS_Lib;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with Coverage;             use Coverage;
with Files_Table;          use Files_Table;
with GNATcov_RTS.Buffers;  use GNATcov_RTS.Buffers;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Outputs;              use Outputs;
with Project;
with System_Commands;      use System_Commands;
with SCOs;
with Strings;              use Strings;
with Switches;             use Switches;
with System;               use System;
with Table;
with Text_Files;           use Text_Files;

package body Instrument.C is

   package GPR renames GNATCOLL.Projects;
   package US renames Ada.Strings.Unbounded;

   ------------------------------
   --  Preprocessing utilities --
   ------------------------------

   Compiler_Macros : Vector_String_Maps.Map;
   --  Cache for computed compiler builtin macros, to avoid relaunching the
   --  compiler command every time a file is instrumented.

   --  The three following functions are not used, but could be in the future,
   --  when we will refine what is done with macros.

   function Builtin_Macros
     (Info : Project_Info;
      Compiler : String) return String_Vectors.Vector;
   --  Return the list of built-in macros for the given compiler. The result is
   --  the list of macro definitions under the following string format:
   --
   --  #define __unix__ 1
   --  ...
   --  #define __INT_WIDTH__ 32

   function Undef_Switches
     (Info     : Project_Info;
      Compiler : String) return String_Vectors.Vector;
   --  Return the list of switches to pass to the preprocessing command to
   --  undefine the builtin macros of the given compiler.
   --
   --  Undefining the __clang__ macro means passing -U__clang__ to the
   --  preprocessing command.

   function Def_Switches
     (Info     : Project_Info;
      Compiler : String) return String_Vectors.Vector;
   --  Return the list of switches to pass to the preprocessing command to
   --  define the builtin macros of the given compiler.
   --
   --  Defining the __GCC_IEC_559 macro with a value of 2 means passing
   --  -D__GCC_IEC_559=2 to the preprocessing command.

   function Preprocessing_Command
     (Info : in out Project_Info;
      Filename : String) return Command_Access;
   --  Return the command to preprocess the file. It is mandatory to do so as
   --  clang rewriter does not accept to rewrite preprocessed code sections.
   --
   --  We will use the compiler in the Compiler_Driver attribute, that we get
   --  from GNATCOLL.Project, to preprocess the file. We will assume that it
   --  accepts the -E flag, to preprocess a file.
   --
   --  TODO: we need the full set of include flags to correctly preprocess
   --  files, and augment the preprocessing command with it. The only thing it
   --  does right now, is to augment the preprocessing command with the source
   --  directories given in the project file, which is not enough.

   -------------------------------------
   -- Generation of witness fragments --
   -------------------------------------

   function Make_Expr_Witness
     (UIC    : C_Unit_Inst_Context;
      Bit    : Bit_Id) return String;
   --  Create a procedure call expression on to witness execution of the low
   --  level SCO with the given bit id.

   function Make_Statement_Witness
     (UIC    : C_Unit_Inst_Context;
      Bit    : Bit_Id) return String;
   --  Create a procedure call statement to witness execution of the low level
   --  SCO with the given bit id.

   procedure Insert_Decision_Witness
     (IC         : in out C_Unit_Inst_Context;
      SD         : C_Source_Decision;
      Path_Count : Positive);
   --  For use when decision coverage or MC/DC is requested. Insert witness
   --  function call for the identified decision.

   procedure Insert_Condition_Witness
     (IC     : in out C_Unit_Inst_Context;
      SC     : C_Source_Condition;
      Offset : Natural);
   --  For use when MC/DC coverage requested. Insert witness function call for
   --  the identified condition.

   function Make_MCDC_State_Name (LL_SCO_Id : Nat) return String is
     ("mcdc_state_" & Img (Integer (LL_SCO_Id)));
   --  Return the name of the MC/DC state local variable for the given
   --  decision SCO.

   function Insert_MCDC_State
     (UIC  : in out C_Unit_Inst_Context'Class;
      Name : String) return String;

   ----------------------------
   -- Source level rewriting --
   ----------------------------

   procedure Initialize_Rewriting
     (IC                : in out C_Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Filename          : String);
   --  Initialize a unit instrumentation context for the given unit to
   --  instrument.

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

   procedure Emit_C_Buffer_Unit
     (Info : in out Project_Info; UIC : C_Unit_Inst_Context'Class);
   --  Emit the unit to contain coverage buffers for the given instrumented
   --  unit.

   procedure Emit_Ada_Buffer_Unit
     (Info : in out Project_Info; UIC : C_Unit_Inst_Context'Class);
   --  Emit the ada buffer unit that contain references to the coverage buffers
   --  defined in the buffer unit.

   procedure Run_Diagnostics (TU : Translation_Unit_T);
   --  Output clang diagnostics on the given translation unit
   pragma Unreferenced (Run_Diagnostics);

   -----------------------
   -- Make_Expr_Witness --
   -----------------------

   function Make_Expr_Witness
     (UIC    : C_Unit_Inst_Context;
      Bit    : Bit_Id) return String
   is
      Bit_Img : constant String  := Img (Bit);
   begin
      return "gnatcov_rts_witness ((void *)"
        & Statement_Buffer_Symbol (UIC.Instrumented_Unit) & "," & Bit_Img
        & ")";
   end Make_Expr_Witness;

   ----------------------------
   -- Make_Statement_Witness --
   ----------------------------

   function Make_Statement_Witness
     (UIC    : C_Unit_Inst_Context;
      Bit    : Bit_Id) return String
   is
      Bit_Img : constant String  := Img (Bit);
   begin
      return Make_Expr_Witness (UIC, Bit) & ";";
   end Make_Statement_Witness;

   ------------------------------
   -- Insert_Condition_Witness --
   ------------------------------

   procedure Insert_Condition_Witness
     (IC     : in out C_Unit_Inst_Context;
      SC     : C_Source_Condition;
      Offset : Natural)
   is
   begin
      --  No instrumentation for condition if there is no local state variable

      if US.Length (SC.State) = 0 then
         return;
      end if;

      declare
         First_Image : constant String :=
           Integer'Image (if SC.First then 1 else 0);
      begin
         Insert_Text_Before (N    => SC.Condition,
                             Text => "gnatcov_rts_witness_condition ((void *)"
                             & US.To_String (SC.State) & ", " & Img (Offset)
                             & ", " & First_Image & ", ",
                             Rew  => IC.Rewriter);
         Insert_Text_After (N    => SC.Condition,
                            Text => ")",
                            Rew  => IC.Rewriter);
      end;
   end Insert_Condition_Witness;

   -----------------------------
   -- Insert_Decision_Witness --
   -----------------------------

   procedure Insert_Decision_Witness
     (IC         : in out C_Unit_Inst_Context;
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
        (False => IC.Unit_Bits.Last_Outcome_Bit + 1,
         True  => IC.Unit_Bits.Last_Outcome_Bit + 2);
      IC.Unit_Bits.Last_Outcome_Bit :=
        IC.Unit_Bits.Last_Outcome_Bit + 2;

      --  Allocate path bits for MC/DC if MC/DC is required and we were
      --  able to generate a local state variable.

      if MCDC_Coverage_Enabled and then US.Length (SD.State) > 0 then
         Bits.Path_Bits_Base := IC.Unit_Bits.Last_Path_Bit + 1;
         IC.Unit_Bits.Last_Path_Bit :=
           IC.Unit_Bits.Last_Path_Bit + Bit_Id (Path_Count);
      else
         Bits.Path_Bits_Base := No_Bit_Id;
      end if;

      IC.Unit_Bits.Decision_Bits.Append (Bits);

      --  Now attach witness call at the place of the original decision

      declare
         Is_MCDC       : constant Boolean := Bits.Path_Bits_Base /= No_Bit_Id;
         Function_Name : constant String :=
           (if Is_MCDC
            then "gnatcov_rts_witness_decision_mcdc"
            else "gnatcov_rts_witness_decision");
         Buffer_Symbol : constant String :=
           (if Is_MCDC
            then MCDC_Buffer_Symbol (IC.Instrumented_Unit)
            else Decision_Buffer_Symbol (IC.Instrumented_Unit));
      begin
         Insert_Text_Before (N    => N,
                             Text => Function_Name & "((void *)"
                             & Decision_Buffer_Symbol (IC.Instrumented_Unit)
                             & ", " & Img (Bits.Outcome_Bits (False)) & ", "
                             & Img (Bits.Outcome_Bits (True)),
                             Rew  => IC.Rewriter);

         if Is_MCDC then
            Insert_Text_Before (N    => N,
                                Text => ", "
                                & MCDC_Buffer_Symbol (IC.Instrumented_Unit)
                                & ", " & Img (Bits.Path_Bits_Base) & ", "
                                & US.To_String (SD.State),
                                Rew  => IC.Rewriter);
         end if;
         Insert_Text_Before (N    => N,
                             Text => ", ",
                             Rew   => IC.Rewriter);

         Insert_Text_After (N    => N,
                            Text => ")",
                            Rew  => IC.Rewriter);
      end;
   end Insert_Decision_Witness;

   -----------------------
   -- Insert_MCDC_State --
   -----------------------

   function Insert_MCDC_State
     (UIC      : in out C_Unit_Inst_Context'Class;
      Name     : String) return String
   is
      Var_Decl_Img  : constant String :=
        "unsigned " & Name & "_var;";
      Addr_Decl_Img : constant String :=
        "void *" & Name & " = &" & Name & "_var;";

   begin
      Insert_Text_Before_Before (N    => UIC.MCDC_State_Declaration_Node,
                                 Text => Var_Decl_Img & Addr_Decl_Img,
                                 Rew  => UIC.Rewriter);
      return Name;
   end Insert_MCDC_State;

   --------------------------
   -- Initialize_Rewriting --
   --------------------------

   procedure Initialize_Rewriting
     (IC                : in out C_Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Filename          : String)
   is
      CIdx : constant Index_T :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      Buffer_Filename : constant String :=
        To_Symbol_Name (Sys_Buffers) & "_b"
        & Instrumented_Unit_Slug (Instrumented_Unit) & ".c";
      --  Name of the generated source file holding the coverage buffers

   begin
      IC.Instrumented_Unit := Instrumented_Unit;
      IC.Buffer_Unit :=
        CU_Name_For_File (+Buffer_Filename,
                          Instrumented_Unit.Project_Name);
      IC.Pure_Buffer_Unit :=
        CU_Name_For_Unit
          (Buffer_Unit (Instrumented_Unit), GPR.Unit_Spec);

      --  TODO: we should pass the same set of preprocessing options that we
      --  pass to the preprocessing command.

      IC.TU :=
        Parse_Translation_Unit
          (C_Idx => CIdx,
           Source_Filename       => Filename,
           Command_Line_Args     => System.Null_Address,
           Num_Command_Line_Args => 0,
           Unsaved_Files         => null,
           Num_Unsaved_Files     => 0,
           Options               =>
             Translation_Unit_Detailed_Preprocessing_Record);
      IC.Rewriter := CX_Rewriter_Create (IC.TU);
   end Initialize_Rewriting;

   procedure Finalize_Rewriting (UIC : C_Unit_Inst_Context);
   --  Overwrite the preprocessed generated files augmented with
   --  instrumentation.

   ------------------------
   -- Finalize_Rewriting --
   ------------------------

   procedure Finalize_Rewriting (UIC : C_Unit_Inst_Context)
   is
      Ignored_Res : Interfaces.C.int;
   begin
      Ignored_Res :=
        CX_Rewriter_Overwrite_Changed_Files (Rew => UIC.Rewriter);
      Dispose_Translation_Unit (UIC.TU);
   end Finalize_Rewriting;

   type SC_Entry is record
      N : Cursor_T;
      --  Original statement node, providing the source location associated
      --  with the statement SCO.

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
     (IC : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant);

   procedure Process_Decisions
     (UIC : in out C_Unit_Inst_Context;
      N   : Cursor_T;
      T   : Character);
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
     (UIC : in out C_Unit_Inst_Context;
      N   : Cursor_T;
      T   : Character)
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

      function Process_Node
        (N : Cursor_T; Parent : Cursor_T; Client_Data : Client_Data_T)
         return Child_Visit_Result_T with Convention => C;
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
               else pragma Assert (Op_N = "&&");
                  C1 := '&';
               end if;
            end if;

            C2 := ' ';
            Append_SCO
              (C1   => C1,
               C2   => C2,
               From => Sloc (Get_Operator_Loc (N)),
               To   => No_Source_Location,
               Last => False);

            Hash_Entries.Append ((Sloc => Start_Sloc (N),
                                  SCO_Index => SCOs.SCO_Table.Last));

            if not Is_Null (L) then
               Output_Decision_Operand (L);
            end if;
            Output_Decision_Operand (R);

         --  Not a logical operator -> condition

         else
            Output_Element (N);

            if MCDC_Coverage_Enabled then
               UIC.Source_Conditions.Append
                 ((LL_SCO    => SCOs.SCO_Table.Last,
                   Condition => N,
                   State     => MCDC_State,
                   First     => Condition_Count = 0));

               Condition_Count := Condition_Count + 1;
            end if;
         end if;
      end Output_Decision_Operand;

      --------------------
      -- Output_Element --
      --------------------

      procedure Output_Element (N : Cursor_T) is
      begin
         Append_SCO
           (C1   => ' ',
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
         Loc : Source_Location := No_Source_Location;
         --  Node whose Sloc is used for the decision

      begin
         case T is
            when 'I' | 'W' =>
               declare
                  N_Parent : constant Cursor_T := Get_Parent (N);
               begin
                  --  For IF, WHILE the token SLOC is that of the parent
                  --  expression. We use the 'I' marker also for ternary
                  --  operator, so we make sure not to be in that situation
                  --  first.

                  if Kind (N_Parent) /= Cursor_Conditional_Operator then
                     Loc := Start_Sloc (N_Parent);
                  end if;
               end;

            when 'X' =>

               --  For an expression, we will use the sloc of the first
               --  condition. This is done afterwards, when processing the low
               --  level scos in sc_obligations.adb.
               --
               --  cf. the Update_Decision_Sloc procedure.

               null;

            --  No other possibilities

            when others =>
               raise Program_Error;
         end case;

         Append_SCO
           (C1   => T,
            C2   => ' ',
            From => Loc,
            To   => No_Source_Location,
            Last => False);

         Current_Decision := SCOs.SCO_Table.Last;

         if Coverage.Enabled (Coverage.Decision)
            or else MCDC_Coverage_Enabled
         then
            if MCDC_Coverage_Enabled then
               Condition_Count := 0;

               MCDC_State := US.To_Unbounded_String
                 (Insert_MCDC_State
                    (UIC, Make_MCDC_State_Name (SCOs.SCO_Table.Last)));
            end if;

            UIC.Source_Decisions.Append
              ((LL_SCO   => Current_Decision,
                Decision => N,
                State    => MCDC_State));
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

      function Process_Node
        (N           : Cursor_T;
         Parent      : Cursor_T;
         Client_Data : Client_Data_T) return Child_Visit_Result_T
      is
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

   procedure Process_Decisions_Defer (N : Cursor_T; T : Character);
   pragma Inline (Process_Decisions_Defer);
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

      function Visitor
        (N : Cursor_T;
         Parent : Cursor_T;
         Client_Data : Client_Data_T) return Child_Visit_Result_T
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

      function Visitor
        (N : Cursor_T; Parent : Cursor_T; Client_Data : Client_Data_T)
         return Child_Visit_Result_T
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
   begin
      for I in  0 .. (Get_Num_Diagnostics (TU) - 1) loop
         declare
            Diag : constant Diagnostic_T :=
              Get_Diagnostic (Unit  => TU, Index => I);
            Str  : constant String_T :=
              Format_Diagnostic
                (Diagnostic => Diag,
                 Options    => Default_Diagnostic_Display_Options);
         begin
            Outputs.Error ("Error when parsing the file "
                           & Get_C_String (Str));
            Dispose_String (Str);
         end;
      end loop;
   end Run_Diagnostics;

   -------------------------
   -- Traverse_Statements --
   -------------------------

   procedure Traverse_Statements
     (IC : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant)
   is
      Discard_Dom : Dominant_Info;
      pragma Warnings (Off, Discard_Dom);
   begin
      Discard_Dom := Traverse_Statements (IC, UIC, L, D);
   end Traverse_Statements;

   function Traverse_Statements
     (IC : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant) return Dominant_Info
   is
      Current_Dominant : Dominant_Info := D;

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;

      procedure Traverse_One (N : Cursor_T);
      --  Traverse a statement

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
               Extend_Statement_Sequence (N, 'I', UIC);

               declare
                  Then_Part : constant Cursor_T := Get_Then (N);
                  Else_Part : constant Cursor_T := Get_Else (N);
               begin
                  Process_Decisions_Defer (Get_Cond (N), 'I');
                  Set_Statement_Entry;

                  --  Now we traverse the statements in the THEN part

                  Curlify (N   => Then_Part,
                           TU  => UIC.TU,
                           Rew => UIC.Rewriter);
                  Traverse_Statements
                    (IC, UIC,
                     L => To_Vector (Then_Part),
                     D => ('S', N));

                  --  Traverse the ELSE statements if present

                  if not Is_Null (Else_Part) then
                     Curlify (N   => Else_Part,
                              TU  => UIC.TU,
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
               Extend_Statement_Sequence (N, 'C', UIC);
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
                  Curlify (N   => While_Body,
                           TU  => UIC.TU,
                           Rew => UIC.Rewriter);
                  Extend_Statement_Sequence
                    (N, 'W', UIC,
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
                  Curlify (N   => Do_Body,
                           TU  => UIC.TU,
                           Rew => UIC.Rewriter);

                  Traverse_Statements
                    (IC, UIC, To_Vector (Do_Body), No_Dominant);
                  Extend_Statement_Sequence
                    (Do_While, 'W', UIC, Instr_Scheme => Instr_Expr);

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
                    (For_Init, ' ', UIC, Insertion_N => N);
                  Extend_Statement_Sequence
                    (For_Cond, 'F', UIC, Instr_Scheme => Instr_Expr);
                  Process_Decisions_Defer (For_Cond, 'X');

                  Set_Statement_Entry;

                  Current_Dominant := ('S', For_Cond);

                  Current_Dominant :=
                    Traverse_Statements (IC, UIC, For_Body, Current_Dominant);

                  Extend_Statement_Sequence
                    (For_Inc, ' ', UIC, Instr_Scheme => Instr_Expr);

                  Set_Statement_Entry;

                  Current_Dominant := ('S', For_Cond);
               end;

           --  Unconditional goto, which is included in the current statement
           --  sequence, but then terminates it.

            when Cursor_Goto_Stmt | Cursor_Indirect_Goto_Stmt =>
               Extend_Statement_Sequence (N, ' ', UIC);
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

            --  TODO: there are probably missing special statements, such as
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

                  Extend_Statement_Sequence (N, ' ', UIC);

                  --  Process any embedded decisions

                  if Has_Decision (N) then
                     Process_Decisions_Defer (N, 'X');
                  end if;
               end if;
         end case;
      end Traverse_One;

      procedure Set_Statement_Entry is

         SC_Last : constant Types.Int := SC.Last;
         SD_Last : constant Types.Int := SD.Last;

         procedure Insert_Statement_Witness (SCE : SC_Entry; LL_SCO_ID : Nat);

         ------------------------------
         -- Insert_Statement_Witness --
         ------------------------------

         procedure Insert_Statement_Witness (SCE : SC_Entry; LL_SCO_ID : Nat)
         is
            Cursor : constant Cursor_T := SCE.Insertion_N;
            Location : constant Source_Location_T
              := Get_Cursor_Location (Cursor);
            pragma Unreferenced (Location);
         begin
            --  Allocate a bit in the statement coverage buffer, and record
            --  its id in the bitmap.

            UIC.Unit_Bits.Last_Statement_Bit :=
              UIC.Unit_Bits.Last_Statement_Bit + 1;
            UIC.Unit_Bits.Statement_Bits.Append
              ((LL_SCO_ID, Executed => UIC.Unit_Bits.Last_Statement_Bit));

            case SCE.Instr_Scheme is
               when Instr_Expr =>
                  Insert_Text_Before
                    (N    => Cursor,
                     Text => Make_Expr_Witness
                       (UIC => UIC,
                        Bit => UIC.Unit_Bits.Last_Statement_Bit)
                        & " && ",
                     Rew  => UIC.Rewriter);

               when Instr_Stmt =>
                  Insert_Text_Before
                    (N    => Cursor,
                     Text => Make_Statement_Witness
                       (UIC => UIC,
                        Bit => UIC.Unit_Bits.Last_Statement_Bit),
                     Rew  => UIC.Rewriter);
            end case;
         end Insert_Statement_Witness;

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
                  Append_SCO
                    (C1   => '>',
                     C2   => Current_Dominant.K,
                     From => From,
                     To   => To,
                     Last => False);
               end;
            end if;

            declare
               SCE : SC_Entry renames SC.Table (J);

            begin
               Append_SCO
                 (C1   => 'S',
                  C2   => SCE.Typ,
                  From => SCE.From,
                  To   => SCE.To,
                  Last => (J = SC_Last));

               Insert_Statement_Witness (SCE, SCOs.SCO_Table.Last);
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

   --  Traverse a translation unit (top level declarations)

   procedure Traverse_Declarations
     (IC  : in out Inst_Context;
      UIC : in out C_Unit_Inst_Context;
      L   : Cursor_Vectors.Vector;
      D   : Dominant_Info := No_Dominant)
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

   -------------------------------
   -- Extend_Statement_Sequence --
   -------------------------------

   procedure Extend_Statement_Sequence
     (N            : Cursor_T;
      Typ          : Character;
      UIC          : C_Unit_Inst_Context;
      Insertion_N  : Cursor_T := Get_Null_Cursor;
      Instr_Scheme : Instr_Scheme_Type := Instr_Stmt)
   is
      Insert_Cursor : aliased Cursor_T := N;

      F : constant Source_Location := Start_Sloc (N);
      T : Source_Location := End_Sloc (N);

      --  Source location bounds used to produce a SCO statement. By
      --  default, this should cover the same source location range as N,
      --  however for nodes that can contain other statements, we select an end
      --  bound that appears before the first nested statement (see To_Node
      --  below).

      To_Node : Cursor_T := Get_Null_Cursor;
      --  In the case of simple statements, set to null cursor and unused.
      --  Otherwise, use F and this node's end sloc for the emitted
      --  statement source location range.

   begin

      if Is_Null (N) then
         return;
      end if;

      --  For now, instrument only cursor that come from the file being
      --  instrumented, and do not instrument included code.

      if Is_Unit_Of_Interest (N, +UIC.File) then
         case Kind (N) is
            when Cursor_If_Stmt =>
               To_Node := Get_Cond (N);
            when Cursor_Switch_Stmt =>
               To_Node := Get_Cond (N);
            when Cursor_While_Stmt =>
               Insert_Cursor := Get_Cond (N);
               To_Node := Insert_Cursor;
            when others => null;
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
      end if;
   end Extend_Statement_Sequence;

   --------------------
   -- Builtin_Macros --
   --------------------

   function Builtin_Macros
     (Info : Project_Info;
      Compiler : String) return String_Vectors.Vector
   is
      Basename : constant String :=
        Ada.Directories.Simple_Name (Compiler) & "_builtins";
      Filename : constant String :=
        (+Info.Output_Dir) / Basename;
      File     : Ada.Text_IO.File_Type;
      Res      : String_Vectors.Vector;
   begin

      if not Compiler_Macros.Contains (+Compiler) then
         declare
            Compiler_Command : constant Command_Access := new Command_Type;
            Macros   : String_Vectors.Vector;
         begin
            Compiler_Command.Command := +Compiler;

            Append_Arg (Compiler_Command, "-E");
            Append_Arg (Compiler_Command, "-dM");
            Append_Arg (Compiler_Command, "-");

            Compiler_Command.Input := +("" & Types.EOF);

            System_Commands.Run_Command
              (Command             => Compiler_Command.all,
               Origin_Command_Name => "gnatcov instrument",
               Output_File         => Filename);

            --  Then, read all the macros in the output file, and store them in
            --  the cache.

            Open (File, In_File, Filename);
            while not End_Of_File (File) loop
               Macros.Append (+Get_Line (File));
            end loop;
            Close (File);
            Delete_File (Filename);
            Compiler_Macros.Insert (+Compiler, Macros);
         end;
      end if;
      Res := Compiler_Macros.Element (+Compiler);
      return Res;
   end Builtin_Macros;

   --------------------
   -- Undef_Switches --
   --------------------

   function Undef_Switches
     (Info     : Project_Info;
      Compiler : String) return String_Vectors.Vector is
      use Ada.Characters;
      use GNAT;

      Macros : constant String_Vectors.Vector :=
        Builtin_Macros (Info, Compiler);
      Res    : String_Vectors.Vector;
   begin
      for Macro of Macros loop
         declare
            Subs : String_Split.Slice_Set;
            Seps : constant String := " " & Latin_1.HT;
         begin
            String_Split.Create (S          => Subs,
                                 From       => +Macro,
                                 Separators => Seps,
                                 Mode       => String_Split.Multiple);

            declare
               Macro : constant String := String_Split.Slice (Subs, 2);

               --  If this is a function-like macro, arrrange to remove the
               --  parenthesis when undefining it.

               Paren_Index : constant Integer :=
                 Ada.Strings.Fixed.Index (Macro, "(");
            begin
               if Paren_Index /= 0 then
                  Res.Append (+("-U"
                              & Macro (Paren_Index .. Macro'Length)));
               else
                  Res.Append (+("-U" & Macro));
               end if;
            end;
         end;
      end loop;
      return Res;
   end Undef_Switches;

   ------------------
   -- Def_Switches --
   ------------------

   function Def_Switches
     (Info     : Project_Info;
      Compiler : String) return String_Vectors.Vector is
      use Ada.Characters;
      use GNAT;

      Macros : constant String_Vectors.Vector :=
        Builtin_Macros (Info, Compiler);
      Res    : String_Vectors.Vector;
   begin
      for Macro of Macros loop
         declare
            Subs : String_Split.Slice_Set;
            Seps : constant String := " " & Latin_1.HT;

            Macro_Name  : US.Unbounded_String;
            Macro_Value : US.Unbounded_String;
         begin
            String_Split.Create (S          => Subs,
                                 From       => +Macro,
                                 Separators => Seps,
                                 Mode       => String_Split.Multiple);

            Macro_Name := +(String_Split.Slice (Subs, 2));

            for I in 3 .. String_Split.Slice_Count (Subs) loop

               --  Everything after the macro name is its value

               US.Append (Macro_Value, +(String_Split.Slice (Subs, I) & " "));
            end loop;

            Res.Append (+("-D" & (+Macro_Name) & "=" & (+Macro_Value)));
         end;
      end loop;
      return Res;
   end Def_Switches;

   ---------------------------
   -- Preprocessing_Command --
   ---------------------------

   function Preprocessing_Command
     (Info     : in out Project_Info;
      Filename : String) return Command_Access
   is
      Result : constant Command_Access := new Command_Type;

      Compiler_Driver : constant String :=
        GNATCOLL.Projects.Attribute_Value
          (Info.Project, GPR.Compiler_Driver_Attribute, "C");

      File : Ada.Text_IO.File_Type;
      Res  : String_Vectors.Vector;
   begin
      Result.Command := +Compiler_Driver;

      Append_Arg (Result, "-E");

      --  Then, we augment the preprocessing command with the other projects
      --  source directories.
      --
      --  TODO: we should probably enhance it with the -I, -D and -U flags
      --  found in the gpr file.

      for Dir of Info.Project.Source_Dirs loop
         Append_Arg (Result, "-I" &
                     (GNATCOLL.VFS."+" (GNATCOLL.VFS.Dir_Name (Dir))));
      end loop;
      Result.Native := True;
      Append_Arg (Result, Filename);
      return Result;
   end Preprocessing_Command;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self           : out C_Source_Rewriter;
      IC             : in out Inst_Context;
      Info           : in out Project_Info;
      Input_Filename : String)
   is
      CIdx : constant Index_T :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);

      --  The command to preprocess the file.
      --  Calls clang preprocessing command.
      Cmd : constant Command_Access := Preprocessing_Command
        (Info, Input_Filename);
      Output_Filename : constant String :=
        Register_New_File (Info, Input_Filename);
   begin
      System_Commands.Run_Command
        (Cmd.all, "gnatcov instrument", Output_Filename, False);

      Self.TU :=
        Parse_Translation_Unit
          (C_Idx => CIdx,
           Source_Filename       => Output_Filename,
           Command_Line_Args     => System.Null_Address,
           Num_Command_Line_Args => 0,
           Unsaved_Files         => null,
           Num_Unsaved_Files     => 0,
           Options               =>
             Translation_Unit_Detailed_Preprocessing_Record);
      Self.Rewriter := CX_Rewriter_Create (Self.TU);
   end Start_Rewriting;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out C_Source_Rewriter) is
      Ignored_Res : Interfaces.C.int;
   begin
      Ignored_Res :=
        CX_Rewriter_Overwrite_Changed_Files (Rew => Self.Rewriter);
      Dispose_Translation_Unit (Self.TU);
   end Apply;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out C_Source_Rewriter) is
   begin
      null;
   end Finalize;

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
      Rewriter : C_Source_Rewriter;

      Orig_Filename : constant String  := +Unit_Info.Filename;
      Base_Filename : constant String :=
        Ada.Directories.Simple_Name (Orig_Filename);

      Cmd : constant Command_Access :=
        Preprocessing_Command (Prj_Info, Orig_Filename);
      --  The command to preprocess the file. Calls clang preprocessing
      --  command.

      Output_File     : Text_Files.File_Type;
      Output_Filename : constant String :=
        Register_New_File (Prj_Info, Orig_Filename);
      --  Where the preprocessed file is output. This is the file we will use
      --  for instrumentation purposes.

      CIdx : constant Index_T :=
        Create_Index
          (Exclude_Declarations_From_PCH => 0, Display_Diagnostics => 0);
      Root_Cursor : Cursor_T;
      Filename : constant String := Orig_Filename;
   begin
      --  Make sure that the simple name of the instrumented source file is
      --  registered in our tables. This is required to properly detect when we
      --  try to load SCOs for the same unit from an ALI file, as ALI files
      --  only provide simple names.

      System_Commands.Run_Command
        (Cmd.all, "gnatcov instrument", Output_Filename, False);
      SCOs.Initialize;
      UIC.SFI := Get_Index_From_Generic_Name
        (Filename,
         Kind                => Files_Table.Source_File,
         Indexed_Simple_Name => True);

      Initialize_Rewriting
        (IC => UIC, Instrumented_Unit => CU_Name, Filename => Output_Filename);
      Root_Cursor := Get_Translation_Unit_Cursor (UIC.TU);
      UIC.File := +Orig_Filename;

      --  Traverse declaration and statements in a translation unit

      Traverse_Declarations
        (IC  => IC,
         UIC => UIC,
         L   => Get_Children (Root_Cursor));

      SCOs.SCO_Unit_Table.Append
        ((File_Name  => new String'(Filename),
          File_Index => UIC.SFI,
          Dep_Num    => 1,
          From       => SCOs.SCO_Table.First,
          To         => SCOs.SCO_Table.Last));

      --  Convert low level SCOs from the instrumenter to high level SCOs.
      --  This creates BDDs for every decision.

      declare
         SCO_Map       : aliased LL_HL_SCO_Map :=
           (SCOs.SCO_Table.First .. SCOs.SCO_Table.Last => No_SCO_Id);
         Bit_Maps      : CU_Bit_Maps;
         Created_Units : Created_Unit_Maps.Map;
      begin
         Process_Low_Level_SCOs
           (Provider      => Instrumenter,
            Origin        => UIC.SFI,
            Created_Units => Created_Units,
            SCO_Map       => SCO_Map'Access,
            Count_Paths   => True);

         --  In the instrumentation case, the origin of SCO information is
         --  the original source file.

         UIC.CU := Created_Units.Element (UIC.SFI);

         if Coverage.Enabled (Coverage.Decision) or else MCDC_Coverage_Enabled
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

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned char *"
                  & Statement_Buffer_Symbol (UIC.Instrumented_Unit) & ";");

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned char *"
                  & Decision_Buffer_Symbol (UIC.Instrumented_Unit) & ";");

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned char *"
                  & MCDC_Buffer_Symbol (UIC.Instrumented_Unit) & ";");

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned gnatcov_rts_witness"
                  & " (void *buffer_address, unsigned bit_id);");

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned gnatcov_rts_witness_decision"
                  & " (void *buffer_address, unsigned false_bit,"
                  & " unsigned true_bit, unsigned value);");

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned gnatcov_rts_witness_decision_mcdc"
                  & "(void *decision_buffer_address,"
                  & " unsigned false_bit, unsigned true_bit,"
                  & " void *mcdc_buffer_address, unsigned mcdc_base,"
                  & " void *mcdc_path_address, unsigned value);");

      Add_Export (UIC.TU,
                  UIC.Rewriter,
                  "extern unsigned gnatcov_rts_witness_condition"
                  & "(unsigned *mcdc_path_address,"
                  & "unsigned offset_for_true,"
                  & " unsigned first, unsigned value);");

      --  Insert automatic buffer dump calls, if requested

      if IC.Dump_Config.Trigger /= Manual and then Unit_Info.Is_Main then
         Rewriter.TU := UIC.TU;
         Rewriter.Rewriter := UIC.Rewriter;
         Add_Auto_Dump_Buffers
           (IC   => IC,
            Info => Prj_Info,
            Main => UIC.Instrumented_Unit,
            Rew => Rewriter);
      end if;
      Finalize_Rewriting (UIC);
      Dispose_Index (CIdx);
   end Instrument_Source_File;

   --------------------------
   -- Unit instrumentation --
   --------------------------

   function Unit_Buffers_Name (Unit : Compilation_Unit_Name) return String;
   --  Name of the buffers struct for this unit

   -----------------------
   -- Unit_Buffers_Name --
   -----------------------

   function Unit_Buffers_Name (Unit : Compilation_Unit_Name) return String is
   begin
      return To_Symbol_Name (Sys_Buffers) & "_" & Instrumented_Unit_Slug (Unit)
        & "_buffers";
   end Unit_Buffers_Name;

   ------------------------
   -- Emit_C_Buffer_Unit --
   ------------------------

   procedure Emit_C_Buffer_Unit
     (Info : in out Project_Info; UIC : C_Unit_Inst_Context'Class)
   is
      use Ada.Strings.Unbounded;
      CU_Name : Compilation_Unit_Name renames UIC.Buffer_Unit;
      Filename : constant String :=
        Instrumented_Unit_Slug (CU_Name) & "_buffers.c";
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
      Create_File (Info, File, Filename);

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
         --  Turn the fingerprint value into the corresponding Ada literal

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
         File.Put_Line ("#include ""gnatcov_rts_c_buffers.h""");
         File.New_Line;
         File.Put_Line
           ("unsigned char " & Statement_Buffer_Repr & "["
            & Img (Any_Bit_Id'Max (1, UIC.Unit_Bits.Last_Statement_Bit + 1))
            & "];");
         File.Put_Line
           ("unsigned char *const " & Statement_Buffer & " = &"
            & Statement_Buffer_Repr & "[0];");
         File.New_Line;

         File.Put_Line
           ("unsigned char " & Decision_Buffer_Repr & "["
            & Img (Any_Bit_Id'Max (1, UIC.Unit_Bits.Last_Outcome_Bit + 1))
            & "];");
         File.Put_Line
           ("unsigned char *const " & Decision_Buffer & " = &"
            & Decision_Buffer_Repr & "[0];");
         File.New_Line;

         File.Put_Line
           ("unsigned char " & MCDC_Buffer_Repr & "["
            & Img (Any_Bit_Id'Max (1, UIC.Unit_Bits.Last_Path_Bit + 1))
            & "];");
         File.Put_Line
           ("unsigned char *const " & MCDC_Buffer & " = &" & MCDC_Buffer_Repr
            & "[0];");
         File.New_Line;

         File.Put_Line ("struct gnatcov_rts_unit_coverage_buffers "
                        & Unit_Buffers_Name (UIC.Instrumented_Unit)
                        & "= {");
         File.Put_Line ("    .fingerprint = " & To_String (Fingerprint) & ",");

         File.Put_Line ("    .language = "
                        & Integer'Image
                          (Any_Language_Kind'Pos (File_Based_Language))
                        & ",");
         File.Put_Line ("    .unit_part = "
                        & Integer'Image
                          (Any_Unit_Part'Pos (Not_Applicable_Part))
                        & ",");
         File.Put_Line ("    .unit_name = " & """" & Unit_Name & """" & ",");
         File.Put_Line ("    .unit_name_length = "
                        & Strings.Img (Unit_Name'Length) & ",");

         File.Put_Line ("    .project_name = " & """" & Project_Name & """"
                        & ",");
         File.Put_Line ("    .project_name_length = "
                        & Strings.Img (Project_Name'Length) & ",");

         --  We do not use the created pointer (Statement_Buffer) to initialize
         --  the buffer fields, as this is rejected by old versions of the
         --  compiler (up to the 20 version): the initializer element is
         --  considered not constant. To work around it, we simply use the
         --  original expression instead of using a wrapper pointer.

         File.Put_Line ("    .statement = &" & Statement_Buffer_Repr & "[0],");
         File.Put_Line ("    .decision = &" & Decision_Buffer_Repr & "[0],");
         File.Put_Line ("    .mcdc = &" & MCDC_Buffer_Repr & "[0],");

         File.Put_Line ("    .statement_last_bit = " & Statement_Last_Bit
                        & ",");
         File.Put_Line ("    .decision_last_bit = " & Decision_Last_Bit
                        & ",");
         File.Put_Line ("    .mcdc_last_bit = " & MCDC_Last_Bit);

         File.Put_Line ("};");
      end;
   end Emit_C_Buffer_Unit;

   --------------------------
   -- Emit_Ada_Buffer_Unit --
   --------------------------

   procedure Emit_Ada_Buffer_Unit
     (Info : in out Project_Info; UIC : C_Unit_Inst_Context'Class)
   is
      use Ada.Strings.Unbounded;
      CU_Name : Compilation_Unit_Name renames UIC.Pure_Buffer_Unit;
      File    : Text_Files.File_Type;
      Project_Name : constant String := +UIC.Instrumented_Unit.Project_Name;
   begin
      Create_File (Info,
                   File,
                   To_Filename (Info.Project, CU_Name, Ada_Language));
      Put_Warnings_And_Style_Checks_Pragmas (File);

      declare
         Pkg_Name : constant String := To_Ada (CU_Name.Unit);

         Fingerprint : US.Unbounded_String;

         Unit_Name : constant String :=
           To_Filename (Info.Project, UIC.Instrumented_Unit, C_Language);

         Statement_Last_Bit : constant String := Img
           (UIC.Unit_Bits.Last_Statement_Bit);
         Decision_Last_Bit  : constant String := Img
           (UIC.Unit_Bits.Last_Outcome_Bit);
         MCDC_Last_Bit      : constant String := Img
           (UIC.Unit_Bits.Last_Path_Bit);

      begin
         --  Turn the fingerprint value into the corresponding Ada literal

         declare
            First : Boolean := True;
         begin
            Append (Fingerprint, "(");
            for Byte of SC_Obligations.Fingerprint (UIC.CU) loop
               if First then
                  First := False;
               else
                  Append (Fingerprint, ", ");
               end if;
               Append (Fingerprint, Strings.Img (Integer (Byte)));
            end loop;
            Append (Fingerprint, ")");
         end;

         File.Put_Line ("package " & Pkg_Name & " is");
         File.New_Line;
         File.Put_Line ("   Statement_Buffer_Address : constant System.Address"
                        & ";");
         File.Put_Line ("   pragma Import (C, Statement_Buffer_Address, """
                        & Statement_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   Decision_Buffer_Address : constant System.Address"
                        & ";");
         File.Put_Line ("   pragma Import (C, Decision_Buffer_Address, """
                        & Decision_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   MCDC_Buffer_Address : constant System.Address;");
         File.Put_Line ("   pragma Import (C, MCDC_Buffer_Address, """
                        & MCDC_Buffer_Symbol (UIC.Instrumented_Unit)
                        & """);");
         File.New_Line;

         File.Put_Line ("   Buffers : aliased Unit_Coverage_Buffers :=");
         File.Put_Line ("     (Unit_Name_Length => "
                        & Strings.Img (Unit_Name'Length) & ",");
         File.Put_Line ("      Project_Name_Length => "
                        & Strings.Img (Project_Name'Length) & ",");
         File.Put_Line ("      Fingerprint => "
                        & To_String (Fingerprint) & ",");

         File.Put_Line ("      Language_Kind => File_Based_Language,");
         File.Put_Line ("      Unit_Part     => Not_Applicable_Part,");
         File.Put_Line ("      Unit_Name     => """ & Unit_Name & """,");

         File.Put_Line ("      Project_Name => " & """" & Project_Name
                        & """,");

         File.Put_Line ("      Statement => Statement_Buffer_Address,");
         File.Put_Line ("      Decision  => Decision_Buffer_Address,");
         File.Put_Line ("      MCDC      => MCDC_Buffer_Address,");

         File.Put_Line ("      Statement_Last_Bit => " & Statement_Last_Bit
                        & ",");
         File.Put_Line ("      Decision_Last_Bit => " & Decision_Last_Bit
                        & ",");
         File.Put_Line ("      MCDC_Last_Bit => " & MCDC_Last_Bit & ");");
         File.New_Line;
         File.Put_Line ("end " & Pkg_Name & ";");
      end;
   end Emit_Ada_Buffer_Unit;

   ---------------------------
   -- Add_Auto_Dump_Buffers --
   ---------------------------

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : C_Source_Rewriter)
   is
      Buffer_Units : constant Ada_Qualified_Name_Vectors.Vector :=
        Buffer_Units_For_Closure (IC, Main);
      --  List of names for units that contains the buffers to dump

      Helper_Filename : Ada_Qualified_Name;
      --  Name of file to contain helpers implementing the buffers dump

   begin
      if Buffer_Units.Is_Empty then
         return;
      end if;
      Emit_Ada_Dump_Helper_Unit (IC, Info, Main, C_Language, Helper_Filename);
      Add_Export (Rew.TU, Rew.Rewriter, "extern void "
                  & Dump_Procedure_Symbol (Main) & "(void);");

      Add_Export (Rew.TU, Rew.Rewriter, "extern void adainit(void);");
      Add_Statement_In_Main (Rew.TU, Rew.Rewriter, "adainit();");

      case IC.Dump_Config.Trigger is
         when Main_End | Ravenscar_Task_Termination =>
            Add_Statement_Before_Return
              (Fun_Decl  => Get_Main (Rew.TU),
               Rew       => Rew.Rewriter,
               Statement => Dump_Procedure_Symbol (Main) & "();");

         when At_Exit =>
            Add_Export (Rew.TU, Rew.Rewriter,
                        "extern int atexit( void ( * function ) (void) );");

            --  We need to initialize the Ada runtime as we are still using
            --  the Ada gnatcov runtime to dump traces.

            Add_Statement_In_Main
              (Rew.TU, Rew.Rewriter, "atexit ("
               & Dump_Procedure_Symbol (Main) & ");");

         when others =>
            null;
      end case;
   end Add_Auto_Dump_Buffers;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit
     (CU_Name   : Compilation_Unit_Name;
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

      Emit_C_Buffer_Unit (Prj_Info, UIC);

      --  Then, for Ada-compatibity, generate an ada buffer unit that imports
      --  the symbols defined in the C buffer compilation unit in an Ada unit.
      --  These references will be used to dump coverage buffers (as we rely on
      --  an Ada coverage runtime as of right now).

      Emit_Ada_Buffer_Unit (Prj_Info, UIC);

      --  Track which CU_Id maps to which instrumented unit

      Instrumented_Unit_CUs.Insert (CU_Name, UIC.CU);
   end Instrument_Unit;
end Instrument.C;
