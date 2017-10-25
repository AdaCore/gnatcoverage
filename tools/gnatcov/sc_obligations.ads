------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2016, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Streams; use Ada.Streams;

with GNAT.Regexp;

limited with Checkpoints;
with Slocs;       use Slocs;
with Traces;      use Traces;
with Types;       use Types;
with Snames;      use Snames;

package SC_Obligations is

   -----------------------
   -- Compilation units --
   -----------------------

   type CU_Id is new Natural;
   No_CU_Id : constant CU_Id := 0;
   subtype Valid_CU_Id is CU_Id range No_CU_Id + 1 .. CU_Id'Last;

   function Comp_Unit (Src_File : Source_File_Index) return CU_Id;
   --  Return the identifier for the compilation unit containing the given
   --  source, or No_CU_Id if no such LI file has been loaded.

   procedure Set_Unit_Has_Code (CU : CU_Id);
   --  Record the presence of object code for CU

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

   type SCO_Id is new Natural;
   No_SCO_Id : constant SCO_Id := 0;
   subtype Valid_SCO_Id is SCO_Id range No_SCO_Id + 1 .. SCO_Id'Last;

   type SCO_Kind is (Statement, Decision, Condition, Operator);

   procedure Add_Address (SCO : SCO_Id; Address : Pc_Type);
   --  Record Address in SCO's address list

   function Image (SCO : SCO_Id; With_Sloc : Boolean := True) return String;

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

   procedure Iterate (P : access procedure (SCO : SCO_Id));
   --  Execute P for each SCO

   function Last_SCO return SCO_Id;
   --  Return highest allocated SCO Id

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

   type Operand_Position is (Left, Right);

   --  Expose BDD node id type for the benefit of checkpoints

   type BDD_Node_Id is new Natural;
   No_BDD_Node_Id : constant BDD_Node_Id := 0;

   ----------------------------
   -- Accessors for SCO info --
   ----------------------------

   --  All SCOs

   function Kind       (SCO : SCO_Id) return SCO_Kind;
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
   --  if the value does not determine the decision outcome.

   function Value (SCO : SCO_Id) return Tristate;
   --  Value of the condition (True or False) if compile-time known. Unknown
   --  otherwise.

   function Enclosing_Decision (SCO : SCO_Id) return SCO_Id;
   --  Enclosing decision (climbing up the expression tree through operator
   --  SCOs).

   procedure Get_Origin
     (SCO        : SCO_Id;
      Prev_SCO   : out SCO_Id;
      Prev_Value : out Boolean);
   --  For a condition SCO that is part of a decision with no diamond, return
   --  the previous tested condition and the value of that condition causing
   --  the condition denoted by SCO to be evaluated.

   --  Operator SCOs

   function Op_Kind (SCO : SCO_Id) return Operator_Kind;
   function Operand (SCO : SCO_Id; Position : Operand_Position) return SCO_Id;

   --  Decision SCOs

   function Condition (SCO : SCO_Id; Index : Condition_Index) return SCO_Id;
   function Last_Cond_Index (SCO : SCO_Id) return Condition_Index;
   function Degraded_Origins (SCO : SCO_Id) return Boolean;

   function Decision_Outcome (SCO : SCO_Id) return Tristate;
   --  For a decision whose outcome is compile time known, return that outcome;
   --  otherwise return Unknown.

   function Has_Diamond (SCO : SCO_Id) return Boolean;
   --  True if decison's BDD has a diamond, i.e. a node reachable through more
   --  than one path.

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

   procedure Set_Degraded_Origins (SCO : SCO_Id; Val : Boolean := True);

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

   -----------------
   -- Checkpoints --
   -----------------

   procedure Checkpoint_Save (S : access Root_Stream_Type'Class);
   --  Save the current SCOs to S

   procedure Checkpoint_Load
     (S  : access Root_Stream_Type'Class;
      CS : access Checkpoints.Checkpoint_State);
   --  Load checkpointed SCOs from S and merge them in current state

private

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
          Pragma_Polling => False,
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
          Pragma_Max_Queue_Length => False,
          Pragma_Memory_Size => False,
          Pragma_No_Body => False,
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
          Pragma_Postcondition => True,
          Pragma_Post_Class => True,
          Pragma_Refined_Post => True,
          Pragma_Pre => True,
          Pragma_Precondition => True,
          Pragma_Pre_Class => True,
          Pragma_Predicate => True,
          Pragma_Contract_Cases => True,

          Pragma_Loop_Invariant => True,
          Pragma_Invariant => True,
          Pragma_Type_Invariant => True,
          Pragma_Type_Invariant_Class => True,

          Unknown_Pragma => True);

end SC_Obligations;
