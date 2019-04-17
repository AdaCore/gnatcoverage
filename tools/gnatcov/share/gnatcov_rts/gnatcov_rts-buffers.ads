--  This package provides types and subprograms to maintain data about the
--  satisfaction of coverage obligations.

--  This unit needs to be compilable with Ada 95 compilers

with Interfaces;
with System;

package GNATcov_RTS.Buffers is

   pragma Pure;

   --  All data takes the form of big arrays of booleans: coverage buffers. How
   --  to interpret these depend on the type of coverage obligation.
   --
   --  For statement coverage, each statement is assigned such a boolean, which
   --  indicates whether it was executed.
   --
   --  For decision coverage, each decision is assigned two booleans: one which
   --  indicates whether the decision reached the False outcome and another for
   --  the True outcome.

   type Any_Unit_Part is (Unit_Body, Unit_Spec, Unit_Separate);

   type Any_Bit_Id is new Integer;
   subtype Bit_Id is Any_Bit_Id range 0 .. Any_Bit_Id'Last;
   --  Unique identifier for a boolean in a coverage buffer

   type Coverage_Buffer_Type is array (Bit_Id range <>) of Boolean;
   pragma Pack (Coverage_Buffer_Type);

   type Hash_Type is new Interfaces.Unsigned_32;
   --  Hash type to perform consistency checks

   type Unit_Coverage_Buffers (Unit_Name_Length : Positive) is record
      Closure_Hash : Hash_Type;
      --  Hash for the instrumented unit and its complete dependency closure.
      --  This hash is used as a fast way to check that coverage obligations
      --  and coverage data are consistent.

      Unit_Part : Any_Unit_Part;
      Unit_Name : String (1 .. Unit_Name_Length);
      --  Unit kind and name for the instrumented unit. More specifically,
      --  Unit_Name is the fully qualified name of the compilation unit (or
      --  subunit) in lower case. For instance: "foo", "ada.text_io" or
      --  "foo.bar.my_subunit".

      Statement, Decision, MCDC : System.Address;
      --  Addresses of coverage buffers for statement obligations, decision
      --  obligations and MC/DC obligations. The address refer to
      --  Coverage_Buffer_Type arrays whose bounds go from
      --  0 to Unit_Coverage_Buffers.*_Last_Bit.

      Statement_Last_Bit, Decision_Last_Bit, MCDC_Last_Bit : Any_Bit_Id;
      --  Index for the last bits in coverage buffers for statements, decisions
      --  and MC/DC.
   end record;

   -------------------------
   -- Witness subrpograms --
   -------------------------

   --  The following subprograms are called by generated code to record
   --  the execution of constructs.

   --  Note that the Buffer_Address parameters below are required so that:
   --
   --  * functions don't have IN OUT formals, which are illegal before Ada
   --    2012;
   --
   --  * our hack for pure units to be able to reference coverage buffers
   --    (which are global state, i.e. unreachable from pure units) works.

   --  Statements and declarations

   type Witness_Dummy_Type is null record;
   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Witness_Dummy_Type;
   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Boolean;
   procedure Witness (Buffer_Address : System.Address; Bit : Bit_Id);
   --  Set the boolean corresponding to Bit to True in Buffer. The Boolean
   --  version always returns True.

   --  Decisions

   subtype MCDC_State_Type is Any_Bit_Id;

   function Witness
     (Buffer_Address      : System.Address;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean;
   --  If Value is False, set the Boolean corresponding to False_Bit to True in
   --  Buffer. Set the one corresponding to True_Bit otherwise.

   function Witness
     (Decision_Buffer_Address : System.Address;
      False_Bit, True_Bit     : Bit_Id;
      MCDC_Buffer_Address     : System.Address;
      MCDC_Base               : Bit_Id;
      MCDC_Path_Address       : System.Address;
      Value                   : Boolean) return Boolean;
   --  Same as above, and also set the bit determined by MCDC_Base and the
   --  Bit_Id value at MCDC_Path_Address in the buffer at MCDC_Buffer_Address.
   --  Note that MCDC_Path may not be passed by value, because it is not known
   --  until the side effects of the actual for the Value formal have been
   --  evaluated.

   --  Conditions

   function Witness
     (Buffer_Address  : System.Address;
      Offset_For_True : Any_Bit_Id;
      First           : Boolean;
      Value           : Boolean) return Boolean;
   --  Buffer_Address is the address of an MCDC_State_Type local variable.
   --  If First is True, first reset it to 0.
   --  If Value is True, add Offset_For_True.

end GNATcov_RTS.Buffers;
