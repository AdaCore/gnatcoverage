--  This package provides types and subprograms to maintain data about the
--  satisfaction of coverage obligations.

package System.GNATcov.Buffers is

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

   type Bit_Id is new Natural;
   --  Unique identifier for a boolean in a coverage buffer

   type Coverage_Buffer_Type is array (Bit_Id range <>) of Boolean;
   pragma Pack (Coverage_Buffer_Type);

   --  Both Witness subprograms below set boolean corresponding to Bit to True
   --  in Buffer. The function form is used to set booleans in the middle of
   --  declarative parts.

   procedure Witness (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id);

   type Witness_Dummy_Type is null record;
   function Witness
     (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id)
      return Witness_Dummy_Type;

end System.GNATcov.Buffers;
