------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Slocs;         use Slocs;
with Traces;        use Traces;

package SC_Obligations is

   -----------------------
   -- Compilation units --
   -----------------------

   type CU_Id is new Natural;
   No_CU_Id : constant CU_Id := 0;
   subtype Valid_CU_Id is CU_Id range No_CU_Id + 1 .. CU_Id'Last;

   function Comp_Unit (LI_Name : String) return CU_Id;
   --  Return the identifier for a given compilation unit, identified by the
   --  base name of its LI file, or No_CU_Id if no such LI file has been
   --  loaded.

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

   procedure Load_SCOs (ALI_Filename : String);
   --  Load source coverage obligations from ALI_Filename

   procedure Report_SCOs_Without_Code;
   --  Output a list of conditions without associated conditional branches

   procedure Iterate (P : access procedure (SCO : SCO_Id));
   --  Execute P for each SCO

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

   ----------------------------
   -- Accessors for SCO info --
   ----------------------------

   --  All SCOs

   function Kind       (SCO : SCO_Id) return SCO_Kind;
   function First_Sloc (SCO : SCO_Id) return Source_Location;
   function Last_Sloc  (SCO : SCO_Id) return Source_Location;
   function Parent     (SCO : SCO_Id) return SCO_Id;

   --  Statement SCOs

   --  Statement_Kind denotes the various statement kinds identified in SCOs

   type Any_Statement_Kind is
     (No_Statement,
      Type_Declaration,
      Subtype_Declaration,
      Object_Declaration,
      Renaming_Declaration,
      Generic_Instantiation,
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

   function S_Kind (SCO : SCO_Id) return Any_Statement_Kind;
   --  Return the statement kind for SCO, or No_Statement for No_SCO_Id.

   function Is_Disabled_Statement (SCO : SCO_Id) return Boolean;
   --  True for a disabled statement, i.e. a statement that is guaranteed to
   --  never generate any code and is excluded from the scope of coverage
   --  analysis.

   function Previous   (SCO : SCO_Id) return SCO_Id;
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

   function Is_Pragma_Annotate_Xcov (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a pragma Annotate (Xcov)

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

   function Has_Diamond (SCO : SCO_Id) return Boolean;
   --  True if decison's BDD has a diamond, i.e. a node reachable through more
   --  than one path.

   function Enclosing_Statement (SCO : SCO_Id) return SCO_Id;
   --  Enclosing statement (climbing up the tree through any enclosing
   --  conditions). May be No_SCO_Id for decisions that are not part of any
   --  statement (e.g. Entry_Guard).

   function Is_Expression (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a pragma Assert/Pre/Postcondition/Check, or an
   --  expression appearing outside of a control structure.

   function Is_Assertion (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a pragma Assert/Pre/Postcondition/Check, or an
   --  equivalent aspect.

   procedure Set_Degraded_Origins (SCO : SCO_Id; Val : Boolean := True);

end SC_Obligations;
