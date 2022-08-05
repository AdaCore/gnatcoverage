------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

--  Source Coverage Obligations

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Regexp;
with GNAT.SHA1;

with Namet;
with Types; use Types;

limited with Checkpoints;
with GNATcov_RTS.Buffers; use GNATcov_RTS.Buffers;
with Slocs;               use Slocs;
with Strings;             use Strings;
with Traces;              use Traces;

package SC_Obligations is

   --  This unit instantiates containers and we want to avoid too much
   --  performance cost when using references to their elements, so suppress
   --  tampering checks.

   pragma Suppress (Tampering_Check);

   ------------------
   -- Source files --
   ------------------

   package SFI_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Pos,
      Element_Type => Source_File_Index);
   --  Vector of source file indices, used to map dependency indices in an
   --  ALI file to our source file indices.

   subtype SFI_Vector is SFI_Vectors.Vector;

   -----------------------
   -- Compilation units --
   -----------------------

   --  Depending on the context, "unit" may mean different things.
   --
   --  (1) "Compilation unit" has a specific definition in Ada: it can be a
   --      (generic) package/subprogram spec/body, or a subunit.
   --
   --  (2) For GCC/GNAT, there is the notion of "main compilation unit": the
   --      one compilation unit mentionned when running "gcc" that compiles a
   --      group of compilation units (for instance: foo.adb to compile both
   --      foo.ads and foo.adb) into an object file/LI file.
   --
   --  (3) For its user interface ("units of interest", --units, Units
   --      project file attribute, ...), GNATcoverage considers that units map
   --      to (2) for Ada (designated by Ada-like unit name, for instance
   --      "Foo.Bar", case insensitive) and to C source files passed to "gcc"
   --      in order to produce object files (designated by base file name).
   --
   --  (4) In low-level SCOs (gnatutil's scos.ads), a unit is a sequence of
   --      SCOs that relate to the same source file. Note that in that case,
   --      there can be several units that relate to the same source file.
   --
   --  (5) In GNATcoverage's internals (CU_Unit/CU_Info in this package), a
   --      unit maps to a source file, regardless of the language for this
   --      source file, and there is at most one unit for a given source file.

   type SCO_Provider is (Compiler, Instrumenter);

   type CU_Id is new Natural;
   No_CU_Id : constant CU_Id := 0;
   subtype Valid_CU_Id is CU_Id range No_CU_Id + 1 .. CU_Id'Last;

   function Provider (CU : CU_Id) return SCO_Provider;
   --  Return the SCO provider corresponding to the given compilation unit

   type SCOs_Hash is new GNAT.SHA1.Binary_Message_Digest;

   function Fingerprint (CU : CU_Id) return SCOs_Hash;
   --  Hash of SCO info in ALI, for incremental coverage consistency check

   function Comp_Unit (Src_File : Source_File_Index) return CU_Id;
   --  Return the identifier for the compilation unit containing the given
   --  source, or No_CU_Id if no such LI file has been loaded.

   procedure Set_Unit_Has_Code (CU : CU_Id);
   --  Record the presence of object code for CU. For units analyzed through
   --  source instrumentation, this is detected by the presence of coverage
   --  buffers for the unit.

   procedure Report_Units_Without_Code;
   --  Emit an error message for any unit of interest for which no object code
   --  has been seen.

   ---------------
   -- Instances --
   ---------------

   type Inst_Id is new Natural;
   No_Inst_Id : constant Inst_Id := 0;
   subtype Valid_Inst_Id is Inst_Id range No_Inst_Id + 1 .. Inst_Id'Last;

   ---------------------------------
   -- Source Coverage Obligations --
   ---------------------------------

   --  One one side, gnatutil's scos.ads defines data structures for low level
   --  SCO tables: this is SCO_Unit_Table and SCO_Table from the SCOs unit,
   --  i.e. tables to hold the parsing of SCOs from LI files or SCOs that the
   --  source instrumenter creates;
   --
   --  On the other hand, this file defines data structures for high level
   --  tables: SC_Obligations.CU_Vector, .SCO_Vector, etc. Basically, high
   --  level tables are populated only through the importation of data in low
   --  level tables (see Process_Low_Level_SCOs below).
   --
   --  We do this two-step process to keep SCO production simple and to
   --  factorize code as much as possible.

   type SCO_Id is new Natural;
   No_SCO_Id : constant SCO_Id := 0;
   subtype Valid_SCO_Id is SCO_Id range No_SCO_Id + 1 .. SCO_Id'Last;

   type Scope_Entity;
   type Scope_Entity_Acc is access Scope_Entity;
   --  Scope_Entity (SE) stores the SCO range, the name, the sloc and the
   --  child scopes of a SE. Note that we assume that the SCOs of a SE can
   --  be designated by a range. We thus include SCOs belonging to children
   --  SEs.
   --
   --  For Ada, we support a granularity of package / subprogram / task /
   --  entry scopes.
   --
   --  TODO??? implementation for C.
   --
   --  This information is computed by the instrumenters (that know what is
   --  a scope, and what is not).

   procedure Read
     (S : access Root_Stream_Type'Class;
      V : out Scope_Entity_Acc);
   procedure Write (S : access Root_Stream_Type'Class; V : Scope_Entity_Acc);

   for Scope_Entity_Acc'Read use Read;
   for Scope_Entity_Acc'Write use Write;

   procedure Free (Scope_Ent : in out Scope_Entity_Acc);

   package Scope_Entities_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Pos,
      Element_Type => Scope_Entity_Acc);
   subtype Scope_Entities_Vector is Scope_Entities_Vectors.Vector;

   type Scope_Entity is record
      From, To : SCO_Id;
      --  SCO range for this scope. As scope entities are computed during
      --  instrumentation, From and To designates low level SCOs that are then
      --  converted to high level SCOs after processing the low level SCOs.

      Name : Unbounded_String;
      Sloc : Local_Source_Location;
      --  Name (as it appears in the source) and sloc of this scope definition

      Children : Scope_Entities_Vector;
      --  Children scopes

      Parent : Scope_Entity_Acc;
      --  Reference to parent scope, null if this is a top-level scope

   end record;

   No_Scope_Entity : constant Scope_Entity :=
     (From     => No_SCO_Id,
      To       => No_SCO_Id,
      Name     => +"",
      Sloc     => No_Local_Location,
      Children => Scope_Entities_Vectors.Empty_Vector,
      Parent   => null);

   type Any_SCO_Kind is (Removed, Statement, Decision, Condition, Operator);
   subtype SCO_Kind is Any_SCO_Kind range Statement .. Operator;
   --  Removed is used for SCOs coming from C code in static inline functions
   --  present in headers. These SCOs can appear duplicated in multiple
   --  compilation units and we replace all but one of the duplicated entries
   --  with a Removed one.

   procedure Add_Address (SCO : SCO_Id; Address : Pc_Type);
   --  Record Address in SCO's address list

   function Image (SCO : SCO_Id; With_Sloc : Boolean := True) return String;

   function Comp_Unit (SCO : SCO_Id) return CU_Id;
   --  Return the owning compilation unit of a SCO

   function Sloc_To_SCO
     (Sloc              : Source_Location;
      Include_Decisions : Boolean := False) return SCO_Id;
   --  Return the innermost condition or statement SCO (or, if
   --  Include_Decisions is set, the innermost decision) whose range contains
   --  the given sloc. It is an error if multiple such SCOs exist and aren't
   --  nested.
   --  Note: if Sloc has a null column number, returns an unspecified statement
   --  SCO among those covering that line (never a condition).
   --  For No_Location, return No_SCO_Id.

   type Operator_Kind is (Op_Not, Op_And_Then, Op_Or_Else);

   function Has_SCO
     (Sloc_Begin : Source_Location;
      Sloc_End   : Source_Location) return Boolean;
   --  Return True if there is at least one Statement or Condition SCO whose
   --  range has a non-null intersection with Sloc_Begin .. Sloc_End.

   type LL_HL_SCO_Map is array (Nat range <>) of SCO_Id;
   --  Map of low level SCOs to high level SCOs

   package Created_Unit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Source_File_Index,
      Element_Type => CU_Id);
   --  Map source files to corresponding compilation unit. Each such map
   --  relates to a single LI file.
   --
   --  For instance, the Created_Unit_Maps.Map instance for "foo.ali" may
   --  contain associations for "foo.ads", "foo.adb" and "foo-subunit.adb", but
   --  not for "bar.adb".

   procedure Process_Low_Level_SCOs
     (Provider      : SCO_Provider;
      Origin        : Source_File_Index;
      Deps          : SFI_Vector := SFI_Vectors.Empty_Vector;
      Created_Units : out Created_Unit_Maps.Map;
      SCO_Map       : access LL_HL_SCO_Map := null;
      Count_Paths   : Boolean);
   --  Populate high level SCO tables (SC_Vectors, CU_Vector, ... in
   --  SC_Obligations' body) from low level ones (global tables from the SCOs
   --  unit).
   --
   --  Provider determines whether low level SCOs come from the compiler (i.e.
   --  a LI file that the compiler generated) or the source instrumenter (i.e.
   --  a SID file that "gnatcov instrument" created).
   --
   --  Origin must be the file (source file or LI file) that triggered the
   --  creation of the SCOs: this is the LI file whose parsing populated low
   --  level tables for compiler SCOs and the instrumented source file
   --  otherwise.
   --
   --  Deps provides the source files that correspond to each dependency number
   --  in low level tables (SCO_Unit_Table_Entry.Dep_Num). When this mapping is
   --  missing (case of empty vector), use the file name present in low level
   --  tables (SCO_Unit_Table_Entry.File_Name).
   --
   --  This updates Created_Units to map source file to the corresponding CU
   --  for all CU that this procedure creates.
   --
   --  When not null, this procedure fills SCO_Map table with the mapping of
   --  low level SCOs to high level SCOs. Note that in practice, callers should
   --  pass null iff we are not performing source instrumentation, as the SCOs
   --  mapping is always used for SID file production.
   --
   --  If Count_Paths is True, compute the number of BDD evaluation paths for
   --  all decisions. This is useful only to determine the size of MC/DC
   --  coverage buffers for source traces.

   procedure Load_SCOs
     (ALI_Filename         : String;
      Ignored_Source_Files : access GNAT.Regexp.Regexp);
   --  Load source coverage obligations from ALI_Filename. If
   --  Ignored_Source_File is non-null, ignore SCOs that target files whose
   --  names match the accessed pattern.

   procedure Report_SCOs_Without_Code;
   --  Output a list of conditions without associated conditional branches

   procedure Report_Multipath_Decisions;
   --  Output a list of decisions containing multiple paths

   procedure Dump_All_SCOs;
   --  Output all SCOs

   procedure Iterate (P : access procedure (SCO : SCO_Id));
   --  Execute P for each SCO

   function Last_SCO return SCO_Id;
   --  Return highest allocated SCO Id

   function Has_Instrumented_Units return Boolean;
   --  Return whether instrumented units were involved so far in any internal
   --  data structure. This is used to select the version format to use when
   --  saving checkpoints.

   pragma Warnings (Off);
   --  Redefinition of entity names from Standard
   type Tristate is (False, True, Unknown);
   pragma Warnings (On);

   subtype Known_Tristate is Tristate range False .. True;
   --  State of a condition, if known

   To_Tristate : constant array (Boolean) of Known_Tristate :=
                   (False => False, True => True);

   To_Boolean : constant array (Known_Tristate) of Boolean :=
                   (False => False, True => True);

   type Any_Condition_Index is new Integer range -1 .. Integer'Last;
   No_Condition_Index : constant Any_Condition_Index := -1;
   subtype Condition_Index is
     Any_Condition_Index range 0 .. Any_Condition_Index'Last;

   type Condition_Values_Array is array (Condition_Index range <>) of Tristate;

   type Operand_Position is (Left, Right);

   --  Expose BDD node id type for the benefit of checkpoints

   type BDD_Node_Id is new Natural;
   No_BDD_Node_Id : constant BDD_Node_Id := 0;

   --  Outgoing arcs from a BDD node

   subtype Valid_BDD_Node_Id is BDD_Node_Id
   range No_BDD_Node_Id + 1 .. BDD_Node_Id'Last;

   ----------------------------
   -- Accessors for SCO info --
   ----------------------------

   --  All SCOs

   function Kind       (SCO : SCO_Id) return Any_SCO_Kind;
   function First_Sloc (SCO : SCO_Id) return Source_Location;
   function Last_Sloc  (SCO : SCO_Id) return Source_Location;
   function Sloc_Range (SCO : SCO_Id) return Source_Location_Range;
   function Parent     (SCO : SCO_Id) return SCO_Id;

   function Unit_Has_Code (SCO : SCO_Id) return Boolean;
   --  True if object code has been seen for the compilation unit containing
   --  This SCO.

   --  Statement SCOs

   --  Statement_Kind denotes the various statement kinds identified in SCOs

   type Any_Statement_Kind is
     (No_Statement,

      --  Declarations

      Type_Declaration,
      Subtype_Declaration,
      Object_Declaration,
      Renaming_Declaration,
      Generic_Instantiation,
      Other_Declaration,

      --  Proper Ada statements

      Accept_Statement,
      Case_Statement,
      Exit_Statement,
      For_Loop_Statement,
      If_Statement,
      Pragma_Statement,
      Disabled_Pragma_Statement,
      Extended_Return_Statement,
      Select_Statement,
      While_Loop_Statement,
      Degenerate_Subprogram_Statement,
      Other_Statement);

   subtype Statement_Kind is Any_Statement_Kind
     range Any_Statement_Kind'Succ (No_Statement)
        .. Any_Statement_Kind'Last;

   subtype Ada_Statement_Kind is
     Statement_Kind range Accept_Statement .. Other_Statement;
   --  Statements in the Ada RM sense (and also pragmas)

   function S_Kind (SCO : SCO_Id) return Any_Statement_Kind;
   --  Return the statement kind for SCO, or No_Statement for No_SCO_Id.

   function Previous (SCO : SCO_Id) return SCO_Id;
   --  Previous statement in basic block

   procedure Dominant
     (SCO     : SCO_Id;
      Dom_SCO : out SCO_Id;
      Dom_Val : out Boolean);
   --  Return the dominant for SCO, if any.

   --  When SCO is executed:
   --    * If Dom_SCO is a statement, it is guaranteed to have been executed
   --      and Dom_Val is set to an unspecified value
   --    * If Dom_SCO is a decision, it is guaranteed to have been evaluated
   --      with value Dom_Val.

   function Ignore_SCO (SCO : SCO_Id) return Boolean;
   --  True if we ignore the provided SCO for coverage purposes, typically for
   --  pragmas (not real Ada statements in the first place) known not to
   --  generate any executable code, which may be treated as a documentation
   --  item in the source. The input SCO argument is expected to designate a
   --  statement SCO.

   package Non_Instrumented_SCO_Sets is
     new Ada.Containers.Ordered_Sets (Element_Type => SCO_Id);

   procedure Set_Stmt_SCO_Non_Instr (SCO : SCO_Id) with
     Pre => Kind (SCO) = Statement;
   --  Mark this statment SCO as non-instrumented

   procedure Set_Decision_SCO_Non_Instr (SCO : SCO_Id) with
     Pre => Kind (SCO) = Decision;
   --  Mark this decision SCO as non-instrumented for decision coverage

   procedure Set_Decision_SCO_Non_Instr_For_MCDC (SCO : SCO_Id) with
     Pre => Kind (SCO) = Decision;
   --  Mark this decision SCO as non-instrumented for MCDC coverage

   function Stmt_SCO_Instrumented (SCO : SCO_Id) return Boolean with
     Pre => Kind (SCO) = Statement;
   --  Whether this statment SCO was instrumented

   function Decision_SCO_Instrumented (SCO : SCO_Id) return Boolean with
     Pre => Kind (SCO) = Decision;
   --  Whether this decision SCO was instrumented for decision coverage

   function Decision_SCO_Instrumented_For_MCDC
     (SCO : SCO_Id) return Boolean with
     Pre => Kind (SCO) = Decision;
   --  Whether this decision SCO was instrumented for MCDC coverage

   function Is_Pragma_Pre_Post_Condition (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a pragma Pre/Postcondition

   function Handler_Range (SCO : SCO_Id) return Source_Location_Range;
   --  For a statement within an exception handler, return the sloc range of
   --  the (innermost) handler.

   --  Condition SCOs

   function Index (SCO : SCO_Id) return Condition_Index;

   function Next_Condition (SCO : SCO_Id; Value : Boolean) return SCO_Id;
   --  Next condition to be tested, depending of value of this condition,
   --  or No_SCO_Id if the value determines the decision outcome.

   function Outcome (SCO : SCO_Id; Value : Boolean) return Tristate;
   --  Outcome of decision if this condition has the given value, or Unknown
   --  if the value does not determine the decision outcome. (Follows through
   --  constant conditions).

   function Value (SCO : SCO_Id) return Tristate;
   --  Value of the condition (True or False) if compile-time known. Unknown
   --  otherwise.

   function Enclosing_Decision (SCO : SCO_Id) return SCO_Id;
   --  Enclosing decision (climbing up the expression tree through operator
   --  SCOs).

   function Offset_For_True (SCO : SCO_Id) return Natural;
   --  Offset to be added to BDD path index when this condition is True

   procedure Get_Origin
     (SCO        : SCO_Id;
      Prev_SCO   : out SCO_Id;
      Prev_Value : out Boolean);
   --  For a condition SCO that is part of a decision with no multipath,
   --  condition, return the previous tested condition and the value of
   --  that condition causing the condition denoted by SCO to be evaluated.

   --  Operator SCOs

   function Op_Kind (SCO : SCO_Id) return Operator_Kind;

   function Operand (SCO : SCO_Id; Position : Operand_Position) return SCO_Id
      with Pre => Kind (SCO) = Operator;
   --  Return the operand slot indicated by Position in the SCO Operator.

   --  Decision SCOs

   function Condition (SCO : SCO_Id; Index : Condition_Index) return SCO_Id;
   function Last_Cond_Index (SCO : SCO_Id) return Condition_Index;
   function Degraded_Origins (SCO : SCO_Id) return Boolean;

   function Decision_Outcome (SCO : SCO_Id) return Tristate;
   --  For a decision whose outcome is compile time known, return that outcome;
   --  otherwise return Unknown.

   function Has_Multipath_Condition (SCO : SCO_Id) return Boolean;
   --  True if decison's BDD has a node reachable through more than one path

   function Enclosing_Statement (SCO : SCO_Id) return SCO_Id;
   --  Enclosing statement (climbing up the tree through any enclosing
   --  conditions). May be No_SCO_Id for decisions that are not part of any
   --  statement (e.g. Entry_Guard). Returns SCO itself if it is a Statement.

   function Is_Expression (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a pragma Assert/Pre/Postcondition/Check, or an
   --  expression appearing outside of a control structure.

   function Is_Assertion (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a pragma Assert/Pre/Postcondition/Check, or an
   --  equivalent aspect.

   function Is_If_Expression (SCO : SCO_Id) return Boolean;
   --  True if SCO is the decision of an IF expression

   function Is_Quantified_Expression (SCO : SCO_Id) return Boolean;
   --  True if SCO is a condition that is a quantified expression

   function Path_Count (SCO : SCO_Id) return Natural;
   --  Return count of paths through decision's BDD from root condition to
   --  any outcome. This should be a positive number for any decision
   --  (at least 2, one for the True outcome, and one for the False outcome),
   --  but this function can also return 0 when the number of paths exceeds
   --  the limit set with Set_Path_Count_Limit.

   procedure Set_Path_Count_Limit (Limit : Natural);
   --  Set the path count limit beyond which BDD path enumeration is aborted

   function Get_Path_Count_Limit return Natural;
   --  Returns the path count limit beyond which BDD path enumartion is aborted

   function Condition_Values
     (SCO        : SCO_Id;
      Path_Index : Natural;
      Outcome    : out Boolean) return Condition_Values_Array;
   --  Return the vector of condition values and outcome for the BDD path
   --  with the given index.

   procedure Set_Degraded_Origins (SCO : SCO_Id; Val : Boolean := True);
   --  Flag SCO to indicate that the value of its (only) condition is known
   --  only modulo an arbitrary negation.

   function Expression_Image (Op_SCO : SCO_Id) return Unbounded_String;
   --  Pretty_print the expression represented by the SCO.

   --------------------------
   -- Sloc -> SCO_Id index --
   --------------------------

   package Sloc_To_SCO_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Local_Source_Location_Range,
      Element_Type => SCO_Id);

   type Sloc_To_SCO_Map_Array is
     array (SCO_Kind) of aliased Sloc_To_SCO_Maps.Map;
   type Sloc_To_SCO_Map_Array_Acc is access all Sloc_To_SCO_Map_Array;
   --  Maps for statement, decision, condition, and operator SCOs

   ---------------------------
   -- Source trace bit maps --
   ---------------------------

   --  For units whose SCOs come from source instrumentation, maintain
   --  mapping of coverage buffer bit indices to SCO info.

   type Statement_Bit_Map is array (Bit_Id range <>) of SCO_Id;
   type Statement_Bit_Map_Access is access all Statement_Bit_Map;
   --  Statement buffer: bit set True denotes that the statement was executed

   type Decision_Bit_Info is record
      D_SCO   : SCO_Id;
      --  Decision SCO

      Outcome : Boolean;
      --  Decision outcome
   end record;

   type Decision_Bit_Map is array (Bit_Id range <>) of Decision_Bit_Info;
   type Decision_Bit_Map_Access is access all Decision_Bit_Map;
   --  Decision buffer: bit set True denotes that the given decision was
   --  evaluated to the given outcome.

   type MCDC_Bit_Info is record
      D_SCO      : SCO_Id;
      --  Decision SCO

      Path_Index : Natural;
      --  BDD path index
   end record;

   type MCDC_Bit_Map is array (Bit_Id range <>) of MCDC_Bit_Info;
   type MCDC_Bit_Map_Access is access all MCDC_Bit_Map;
   --  MCDC buffer: bit set True denotes that the given decision was
   --  evaluated, and that the indicated path through the BDD was taken.

   type CU_Bit_Maps is record
      Statement_Bits : Statement_Bit_Map_Access;
      Decision_Bits  : Decision_Bit_Map_Access;
      MCDC_Bits      : MCDC_Bit_Map_Access;
   end record;

   function Bit_Maps (CU : CU_Id) return CU_Bit_Maps;
   --  For a unit whose coverage is assessed through source code
   --  instrumentation, return bit maps.

   procedure Set_Bit_Maps (CU : CU_Id; Bit_Maps : CU_Bit_Maps);
   --  Set the tables mapping source trace bit indices to SCO discharge info

  --  With languages featuring macros such as C, coverage obligations are
  --  established from expanded code but the user level sources against which
  --  we produce reports are the original unexpanded ones. Here, we instate
  --  mechanisms to let us track and eventually report how expanded SCOs
  --  connect to the original sources.

   type SCO_PP_Kind is (In_Expansion, No_Expansion);

   type Expansion_Info is record
      Macro_Name : Unbounded_String;
      Sloc       : Slocs.Source_Location;
   end record;

   package Expansion_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Expansion_Info);

   type PP_Info (Kind : SCO_PP_Kind := No_Expansion) is record
      Actual_Source_Range : Slocs.Local_Source_Location_Range;
      --  Refers to the source location of a SCO in the unpreprocessed view
      --  of the source file.

      PP_Source_Range     : Slocs.Local_Source_Location_Range;
      --  Refer to a source location from the preprocessed version of the
      --  source file, without accounting for preprocessor-inserted line
      --  directives.

      Expansion_Stack : Expansion_Lists.List;
      --  Empty if PP_Info.Kind = No_Expansion

      case Kind is
         when In_Expansion =>
            Definition_Loc : Expansion_Info;
            --  Location in the definition of the ultimate macro expansion

         when others       =>
            null;
      end case;

   end record;
   --  Preprocessing information for SCOs. Hold basic information to enhance
   --  the report output with more precise messages.

   package SCO_PP_Info_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => SCO_Id,
      Element_Type => PP_Info);

   procedure Add_PP_Info (SCO : SCO_Id; Info : PP_Info)
      with Pre => not Has_PP_Info (SCO);
      --  Add macro expansion information for the given SCO

   function Has_PP_Info (SCO : SCO_Id) return Boolean;
   --  Return whether the given SCO comes has preprocessing information

   function Get_PP_Info (SCO : SCO_Id) return PP_Info
     with Pre => Has_PP_Info (SCO);
   --  Return the preprocessing information (if any) for the given SCO

   function Get_Scope_Entity (CU : CU_Id) return Scope_Entity_Acc;
    --  Return description for top-level scope, if available. Return null
    --  otherwise.

   procedure Set_Scope_Entity (CU : CU_Id; Scope_Ent : Scope_Entity_Acc);
   --  Set the top level scope for the given unit

   -----------------
   -- Checkpoints --
   -----------------

   procedure Checkpoint_Save (CSS : access Checkpoints.Checkpoint_Save_State);
   --  Save the current SCOs to stream

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : access Checkpoints.Checkpoint_Load_State);
   --  Load checkpointed SCOs from stream and merge them in current state

   ---------------------------
   -- Accessors for CU_Info --
   ---------------------------

   function First_SCO (CU : CU_Id) return SCO_Id;
   --  Return the first SCO of the compilation unit

   function Last_SCO (CU : CU_Id) return SCO_Id;
   --  Return the last SCO of the compilation unit

   -------------
   -- Pragmas --
   -------------

   type Pragma_Id is
     (Pragma_Abort_Defer,
      Pragma_Abstract_State,
      Pragma_Ada_05,
      Pragma_Ada_12,
      Pragma_Ada_2005,
      Pragma_Ada_2012,
      Pragma_Ada_2020,
      Pragma_Ada_2022,
      Pragma_Ada_83,
      Pragma_Ada_95,
      Pragma_Aggregate_Individually_Assign,
      Pragma_All_Calls_Remote,
      Pragma_Allow_Integer_Address,
      Pragma_Annotate,
      Pragma_Assert,
      Pragma_Assert_And_Cut,
      Pragma_Assertion_Policy,
      Pragma_Assume,
      Pragma_Assume_No_Invalid_Values,
      Pragma_Async_Readers,
      Pragma_Async_Writers,
      Pragma_Asynchronous,
      Pragma_Atomic,
      Pragma_Atomic_Components,
      Pragma_Attach_Handler,
      Pragma_Attribute_Definition,
      Pragma_CPP_Class,
      Pragma_CPP_Constructor,
      Pragma_CPP_Virtual,
      Pragma_CPP_Vtable,
      Pragma_CPU,
      Pragma_CUDA_Device,
      Pragma_CUDA_Execute,
      Pragma_CUDA_Global,
      Pragma_C_Pass_By_Copy,
      Pragma_Check,
      Pragma_Check_Float_Overflow,
      Pragma_Check_Name,
      Pragma_Check_Policy,
      Pragma_Comment,
      Pragma_Common_Object,
      Pragma_Compile_Time_Error,
      Pragma_Compile_Time_Warning,
      Pragma_Compiler_Unit,
      Pragma_Compiler_Unit_Warning,
      Pragma_Complete_Representation,
      Pragma_Complex_Representation,
      Pragma_Component_Alignment,
      Pragma_Constant_After_Elaboration,
      Pragma_Contract_Cases,
      Pragma_Controlled,
      Pragma_Convention,
      Pragma_Convention_Identifier,
      Pragma_Deadline_Floor,
      Pragma_Debug,
      Pragma_Debug_Policy,
      Pragma_Default_Initial_Condition,
      Pragma_Default_Scalar_Storage_Order,
      Pragma_Default_Storage_Pool,
      Pragma_Depends,
      Pragma_Detect_Blocking,
      Pragma_Disable_Atomic_Synchronization,
      Pragma_Discard_Names,
      Pragma_Dispatching_Domain,
      Pragma_Effective_Reads,
      Pragma_Effective_Writes,
      Pragma_Elaborate,
      Pragma_Elaborate_All,
      Pragma_Elaborate_Body,
      Pragma_Elaboration_Checks,
      Pragma_Eliminate,
      Pragma_Enable_Atomic_Synchronization,
      Pragma_Export,
      Pragma_Export_Function,
      Pragma_Export_Object,
      Pragma_Export_Procedure,
      Pragma_Export_Value,
      Pragma_Export_Valued_Procedure,
      Pragma_Extend_System,
      Pragma_Extensions_Allowed,
      Pragma_Extensions_Visible,
      Pragma_External,
      Pragma_External_Name_Casing,
      Pragma_Fast_Math,
      Pragma_Favor_Top_Level,
      Pragma_Finalize_Storage_Only,
      Pragma_Ghost,
      Pragma_Global,
      Pragma_Gnat_Annotate,
      Pragma_Ident,
      Pragma_Ignore_Pragma,
      Pragma_Implementation_Defined,
      Pragma_Implemented,
      Pragma_Implicit_Packing,
      Pragma_Import,
      Pragma_Import_Function,
      Pragma_Import_Object,
      Pragma_Import_Procedure,
      Pragma_Import_Valued_Procedure,
      Pragma_Independent,
      Pragma_Independent_Components,
      Pragma_Initial_Condition,
      Pragma_Initialize_Scalars,
      Pragma_Initializes,
      Pragma_Inline,
      Pragma_Inline_Always,
      Pragma_Inline_Generic,
      Pragma_Inspection_Point,
      Pragma_Interface,
      Pragma_Interface_Name,
      Pragma_Interrupt_Handler,
      Pragma_Interrupt_Priority,
      Pragma_Interrupt_State,
      Pragma_Invariant,
      Pragma_Keep_Names,
      Pragma_License,
      Pragma_Link_With,
      Pragma_Linker_Alias,
      Pragma_Linker_Constructor,
      Pragma_Linker_Destructor,
      Pragma_Linker_Options,
      Pragma_Linker_Section,
      Pragma_List,
      Pragma_Lock_Free,
      Pragma_Locking_Policy,
      Pragma_Loop_Invariant,
      Pragma_Loop_Optimize,
      Pragma_Loop_Variant,
      Pragma_Machine_Attribute,
      Pragma_Main,
      Pragma_Main_Storage,
      Pragma_Max_Entry_Queue_Depth,
      Pragma_Max_Entry_Queue_Length,
      Pragma_Max_Queue_Length,
      Pragma_Memory_Size,
      Pragma_No_Body,
      Pragma_No_Caching,
      Pragma_No_Component_Reordering,
      Pragma_No_Elaboration_Code_All,
      Pragma_No_Heap_Finalization,
      Pragma_No_Inline,
      Pragma_No_Return,
      Pragma_No_Run_Time,
      Pragma_No_Strict_Aliasing,
      Pragma_No_Tagged_Streams,
      Pragma_Normalize_Scalars,
      Pragma_Obsolescent,
      Pragma_Optimize,
      Pragma_Optimize_Alignment,
      Pragma_Ordered,
      Pragma_Overflow_Mode,
      Pragma_Overriding_Renamings,
      Pragma_Pack,
      Pragma_Page,
      Pragma_Part_Of,
      Pragma_Partition_Elaboration_Policy,
      Pragma_Passive,
      Pragma_Persistent_BSS,
      Pragma_Post,
      Pragma_Post_Class,
      Pragma_Postcondition,
      Pragma_Pre,
      Pragma_Pre_Class,
      Pragma_Precondition,
      Pragma_Predicate,
      Pragma_Predicate_Failure,
      Pragma_Preelaborable_Initialization,
      Pragma_Preelaborate,
      Pragma_Prefix_Exception_Messages,
      Pragma_Priority,
      Pragma_Priority_Specific_Dispatching,
      Pragma_Profile,
      Pragma_Profile_Warnings,
      Pragma_Propagate_Exceptions,
      Pragma_Provide_Shift_Operators,
      Pragma_Psect_Object,
      Pragma_Pure,
      Pragma_Pure_Function,
      Pragma_Queuing_Policy,
      Pragma_Rational,
      Pragma_Ravenscar,
      Pragma_Refined_Depends,
      Pragma_Refined_Global,
      Pragma_Refined_Post,
      Pragma_Refined_State,
      Pragma_Relative_Deadline,
      Pragma_Remote_Access_Type,
      Pragma_Remote_Call_Interface,
      Pragma_Remote_Types,
      Pragma_Rename_Pragma,
      Pragma_Restricted_Run_Time,
      Pragma_Restriction_Warnings,
      Pragma_Restrictions,
      Pragma_Reviewable,
      Pragma_SPARK_Mode,
      Pragma_Secondary_Stack_Size,
      Pragma_Share_Generic,
      Pragma_Shared,
      Pragma_Shared_Passive,
      Pragma_Short_Circuit_And_Or,
      Pragma_Short_Descriptors,
      Pragma_Simple_Storage_Pool_Type,
      Pragma_Source_File_Name,
      Pragma_Source_File_Name_Project,
      Pragma_Source_Reference,
      Pragma_Static_Elaboration_Desired,
      Pragma_Storage_Size,
      Pragma_Storage_Unit,
      Pragma_Stream_Convert,
      Pragma_Style_Checks,
      Pragma_Subprogram_Variant,
      Pragma_Subtitle,
      Pragma_Suppress,
      Pragma_Suppress_All,
      Pragma_Suppress_Debug_Info,
      Pragma_Suppress_Exception_Locations,
      Pragma_Suppress_Initialization,
      Pragma_System_Name,
      Pragma_Task_Dispatching_Policy,
      Pragma_Task_Info,
      Pragma_Task_Name,
      Pragma_Task_Storage,
      Pragma_Test_Case,
      Pragma_Thread_Local_Storage,
      Pragma_Time_Slice,
      Pragma_Title,
      Pragma_Type_Invariant,
      Pragma_Type_Invariant_Class,
      Pragma_Unchecked_Union,
      Pragma_Unevaluated_Use_Of_Old,
      Pragma_Unimplemented_Unit,
      Pragma_Universal_Aliasing,
      Pragma_Universal_Data,
      Pragma_Unmodified,
      Pragma_Unreferenced,
      Pragma_Unreferenced_Objects,
      Pragma_Unreserve_All_Interrupts,
      Pragma_Unsuppress,
      Pragma_Unused,
      Pragma_Use_VADS_Size,
      Pragma_Validity_Checks,
      Pragma_Volatile,
      Pragma_Volatile_Components,
      Pragma_Volatile_Full_Access,
      Pragma_Volatile_Function,
      Pragma_Warning_As_Error,
      Pragma_Warnings,
      Pragma_Weak_External,
      Pragma_Wide_Character_Encoding,
      Unknown_Pragma);
   --  Set of pragmas that GNATcoverage knows (or Unknown_Pragma if not)

   function Case_Insensitive_Get_Pragma_Id
     (Pragma_Name : Namet.Name_Id) return Pragma_Id;
   --  Return the Pragma_Id correspnding to the given pragma name. This takes
   --  care of converting Pragma_Name to lowercase (canonical form for
   --  Snames.Get_Pragma_Id)

   --  For each pragma we know of, whether an occurrence of the pragma in the
   --  source might generate code of its own, e.g. pragma Precondition.
   --
   --  This table is used to determine if a statement SCO attached to such a
   --  pragma may be ignored for coverage analysis purposes. Note that
   --  activation or deactivation (e.g. for assert-like pragmas controlled by
   --  -gnata or configuration) is treated separately. Pragmas which do
   --  generate code when activated should be listed as Might_Generate_Code
   --  here, regardless of their actual activation status.  Conversely,
   --  pragmas that only influence code generation performed otherwise (e.g.
   --  pragma Inline) should be listed as not Might_Generate_Code.

   Pragma_Might_Generate_Code : constant array (Pragma_Id) of Boolean
     := ( --  Configuration pragmas

          Pragma_Ada_83 => False,
          Pragma_Ada_95 => False,
          Pragma_Ada_05 => False,
          Pragma_Ada_2005 => False,
          Pragma_Ada_12 => False,
          Pragma_Ada_2012 => False,
          Pragma_Ada_2020 => False,
          Pragma_Ada_2022 => False,

          Pragma_Aggregate_Individually_Assign => False,
          Pragma_Allow_Integer_Address => False,
          Pragma_Annotate => False,
          Pragma_Assertion_Policy => False,
          Pragma_Assume_No_Invalid_Values => False,
          Pragma_C_Pass_By_Copy => False,
          Pragma_Check_Float_Overflow => False,
          Pragma_Check_Name => False,
          Pragma_Check_Policy => False,
          Pragma_Compile_Time_Error => False,
          Pragma_Compile_Time_Warning => False,
          Pragma_Compiler_Unit => False,
          Pragma_Compiler_Unit_Warning => False,
          Pragma_Component_Alignment => False,
          Pragma_Convention_Identifier => False,
          Pragma_Debug_Policy => False,
          Pragma_Detect_Blocking => False,
          Pragma_Default_Storage_Pool => False,
          Pragma_Disable_Atomic_Synchronization => False,
          Pragma_Discard_Names => False,
          Pragma_Elaboration_Checks => False,
          Pragma_Eliminate => False,
          Pragma_Enable_Atomic_Synchronization => False,
          Pragma_Extend_System => False,
          Pragma_Extensions_Allowed => False,
          Pragma_External_Name_Casing => False,
          Pragma_Favor_Top_Level => False,
          Pragma_Ignore_Pragma => False,
          Pragma_Implicit_Packing => False,
          Pragma_Initialize_Scalars => False,
          Pragma_Interrupt_State => False,
          Pragma_License => False,
          Pragma_Locking_Policy => False,
          Pragma_No_Component_Reordering => False,
          Pragma_No_Heap_Finalization => False,
          Pragma_No_Run_Time => False,
          Pragma_No_Strict_Aliasing => False,
          Pragma_Normalize_Scalars => False,
          Pragma_Optimize_Alignment => False,
          Pragma_Overflow_Mode => False,
          Pragma_Overriding_Renamings => False,
          Pragma_Partition_Elaboration_Policy => False,
          Pragma_Persistent_BSS => False,
          Pragma_Prefix_Exception_Messages => False,
          Pragma_Priority_Specific_Dispatching => False,
          Pragma_Profile => False,
          Pragma_Profile_Warnings => False,
          Pragma_Propagate_Exceptions => False,
          Pragma_Queuing_Policy => False,
          Pragma_Rational => False,
          Pragma_Ravenscar => False,
          Pragma_Rename_Pragma => False,
          Pragma_Restricted_Run_Time => False,
          Pragma_Restrictions => False,
          Pragma_Restriction_Warnings => False,
          Pragma_Reviewable => False,
          Pragma_Short_Circuit_And_Or => False,
          Pragma_Short_Descriptors => False,
          Pragma_Source_File_Name => False,
          Pragma_Source_File_Name_Project => False,
          Pragma_SPARK_Mode => False,
          Pragma_Style_Checks => False,
          Pragma_Suppress => False,
          Pragma_Suppress_Exception_Locations => False,
          Pragma_Task_Dispatching_Policy => False,
          Pragma_Unevaluated_Use_Of_Old => False,
          Pragma_Universal_Data => False,
          Pragma_Unsuppress => False,
          Pragma_Use_VADS_Size => False,
          Pragma_Validity_Checks => False,
          Pragma_Warning_As_Error => False,
          Pragma_Warnings => False,
          Pragma_Wide_Character_Encoding => False,

          --  Remaining (non-configuration) pragmas, not generating code first

          Pragma_Abort_Defer => False,
          Pragma_Abstract_State => False,
          Pragma_All_Calls_Remote => False,
          Pragma_Async_Readers => False,
          Pragma_Async_Writers => False,
          Pragma_Asynchronous => False,
          Pragma_Atomic => False,
          Pragma_Atomic_Components => False,
          Pragma_Attach_Handler => False,
          Pragma_Attribute_Definition => False,
          Pragma_Check => False,
          Pragma_Comment => False,
          Pragma_Common_Object => False,
          Pragma_Complete_Representation => False,
          Pragma_Complex_Representation => False,
          Pragma_Constant_After_Elaboration => False,
          Pragma_Controlled => False,
          Pragma_Convention => False,
          Pragma_CPP_Class => False,
          Pragma_CPP_Constructor => False,
          Pragma_CPP_Virtual => False,
          Pragma_CPP_Vtable => False,
          Pragma_Deadline_Floor => False,
          Pragma_Default_Initial_Condition => False,
          Pragma_Depends => False,
          Pragma_Effective_Reads => False,
          Pragma_Effective_Writes => False,
          Pragma_Elaborate => False,
          Pragma_Elaborate_All => False,
          Pragma_Elaborate_Body => False,
          Pragma_Export => False,
          Pragma_Export_Function => False,
          Pragma_Export_Object => False,
          Pragma_Export_Procedure => False,
          Pragma_Export_Value => False,
          Pragma_Export_Valued_Procedure => False,
          Pragma_Extensions_Visible => False,
          Pragma_External => False,
          Pragma_Finalize_Storage_Only => False,
          Pragma_Ghost => False,
          Pragma_Global => False,
          Pragma_Gnat_Annotate => False,
          Pragma_Ident => False,
          Pragma_Implementation_Defined => False,
          Pragma_Implemented => False,
          Pragma_Import => False,
          Pragma_Import_Function => False,
          Pragma_Import_Object => False,
          Pragma_Import_Procedure => False,
          Pragma_Import_Valued_Procedure => False,
          Pragma_Independent => False,
          Pragma_Independent_Components => False,
          Pragma_Initial_Condition => False,
          Pragma_Initializes => False,
          Pragma_Inline => False,
          Pragma_Inline_Always => False,
          Pragma_Inline_Generic => False,
          Pragma_Inspection_Point => False,
          Pragma_Interface_Name => False,
          Pragma_Interrupt_Handler => False,
          Pragma_Keep_Names => False,
          Pragma_Link_With => False,
          Pragma_Linker_Alias => False,
          Pragma_Linker_Constructor => False,
          Pragma_Linker_Destructor => False,
          Pragma_Linker_Options => False,
          Pragma_Linker_Section => False,
          Pragma_List => False,
          Pragma_Loop_Optimize => False,
          Pragma_Loop_Variant => False,
          Pragma_Machine_Attribute => False,
          Pragma_Main => False,
          Pragma_Main_Storage => False,
          Pragma_Max_Entry_Queue_Depth => False,
          Pragma_Max_Entry_Queue_Length => False,
          Pragma_Max_Queue_Length => False,
          Pragma_Memory_Size => False,
          Pragma_No_Body => False,
          Pragma_No_Caching => False,
          Pragma_No_Elaboration_Code_All => False,
          Pragma_No_Inline => False,
          Pragma_No_Return => False,
          Pragma_No_Tagged_Streams => False,
          Pragma_Obsolescent => False,
          Pragma_Optimize => False,
          Pragma_Ordered => False,
          Pragma_Pack => False,
          Pragma_Page => False,
          Pragma_Part_Of => False,
          Pragma_Passive => False,
          Pragma_Predicate_Failure => False,
          Pragma_Preelaborable_Initialization => False,
          Pragma_Preelaborate => False,
          Pragma_Provide_Shift_Operators => False,
          Pragma_Psect_Object => False,
          Pragma_Pure => False,
          Pragma_Pure_Function => False,
          Pragma_Refined_Depends => False,
          Pragma_Refined_Global => False,
          Pragma_Refined_State => False,
          Pragma_Relative_Deadline => False,
          Pragma_Remote_Access_Type => False,
          Pragma_Remote_Call_Interface => False,
          Pragma_Remote_Types => False,
          Pragma_Share_Generic => False,
          Pragma_Shared => False,
          Pragma_Shared_Passive => False,
          Pragma_Simple_Storage_Pool_Type => False,
          Pragma_Source_Reference => False,
          Pragma_Static_Elaboration_Desired => False,
          Pragma_Stream_Convert => False,
          Pragma_Subprogram_Variant => False,
          Pragma_Subtitle => False,
          Pragma_Suppress_All => False,
          Pragma_Suppress_Debug_Info => False,
          Pragma_Suppress_Initialization => False,
          Pragma_System_Name => False,
          Pragma_Test_Case => False,
          Pragma_Task_Info => False,
          Pragma_Task_Name => False,
          Pragma_Task_Storage => False,
          Pragma_Thread_Local_Storage => False,
          Pragma_Time_Slice => False,
          Pragma_Title => False,
          Pragma_Unchecked_Union => False,
          Pragma_Unimplemented_Unit => False,
          Pragma_Universal_Aliasing => False,
          Pragma_Unmodified => False,
          Pragma_Unreferenced => False,
          Pragma_Unreferenced_Objects => False,
          Pragma_Unreserve_All_Interrupts => False,
          Pragma_Unused => False,
          Pragma_Volatile => False,
          Pragma_Volatile_Components => False,
          Pragma_Volatile_Full_Access => False,
          Pragma_Volatile_Function => False,
          Pragma_Weak_External => False,

          Pragma_CPU => False,
          Pragma_CUDA_Device => False,
          Pragma_CUDA_Execute => False,
          Pragma_CUDA_Global => False,
          Pragma_Default_Scalar_Storage_Order => False,
          Pragma_Dispatching_Domain => False,
          Pragma_Fast_Math => False,
          Pragma_Interface => False,
          Pragma_Interrupt_Priority => False,
          Pragma_Lock_Free => False,
          Pragma_Priority => False,
          Pragma_Secondary_Stack_Size => False,
          Pragma_Storage_Size => False,
          Pragma_Storage_Unit => False,

          --  Special case for pre/postcondition: these do not generate code
          --  at their normal point of occurrence in the instruction flow,
          --  and in the case of instrumentation based coverage, they cannot
          --  be instrumented because of their special placement rules. So,
          --  we mark them as generating no code, and we treat them as
          --  "free-standing" decisions (outside of statement context).

          Pragma_Postcondition => False,
          Pragma_Precondition => False,

          --  Now pragmas which might generate code. This is an explicit list
          --  instead of a mere "others" fallback to make sure we notice when
          --  new pragmas get in the daily compiler from which we build, which
          --  we expect to be reflected through Snames.
          --
          --  The Unknown case is there to handle situations at run time when
          --  reading ali files produced by a version of the compiler more
          --  recent than gnatcov. True is a conservative choice in this case,
          --  as it will lead gnatcov to flag such pragma as real statements
          --  even if they actually never generate code.

          Pragma_Assume => True,
          Pragma_Assert => True,
          Pragma_Assert_And_Cut => True,

          Pragma_Debug => True,
          Pragma_Post => True,
          Pragma_Post_Class => True,
          Pragma_Refined_Post => True,
          Pragma_Pre => True,
          Pragma_Predicate => True,
          Pragma_Pre_Class => True,
          Pragma_Contract_Cases => True,

          Pragma_Loop_Invariant => True,
          Pragma_Invariant => True,
          Pragma_Type_Invariant => True,
          Pragma_Type_Invariant_Class => True,

          Unknown_Pragma => True);

private

   --  Write accessors for child units

   procedure Set_Operand_Or_Expression
     (SCO      : SCO_Id;
      Position : Operand_Position;
      Expr     : SCO_Id)
      with Pre => Kind (SCO) in Operator | Decision;
   --  If SCO is an Operator, set the Operand slot indicated by Position
   --  to Expr.
   --
   --  If SCO is a Decision, Position is ignored, and the decision's
   --  Expression is set to Expr.

   procedure Set_BDD_Node (C_SCO : SCO_Id; BDD_Node : BDD_Node_Id);
   --  Set the BDD node for the given condition SCO

end SC_Obligations;
