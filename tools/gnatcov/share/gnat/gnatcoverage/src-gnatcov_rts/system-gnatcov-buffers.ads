--  This package provides types and subprograms to maintain data about the
--  satisfaction of coverage obligations.

with Interfaces;

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

   type Any_Unit_Part is (Unit_Body, Unit_Spec, Unit_Separate);

   type Any_Bit_Id is new Integer;
   subtype Bit_Id is Any_Bit_Id range 0 .. Any_Bit_Id'Last;
   --  Unique identifier for a boolean in a coverage buffer

   type Coverage_Buffer_Type is array (Bit_Id range <>) of Boolean;
   pragma Pack (Coverage_Buffer_Type);

   type Hash_Type is new Interfaces.Unsigned_32;
   --  Hash type to perform consistency checks

   type Unit_Coverage_Buffers
     (Unit_Name_Length                 : Positive;
      Stmt_Last_Bit, Decision_Last_Bit : Any_Bit_Id)
   is record
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

      Stmt : Coverage_Buffer_Type (0 .. Stmt_Last_Bit) := (others => False);
      Dc   : Coverage_Buffer_Type (0 .. Decision_Last_Bit) :=
         (others => False);
      --  Coverage buffers for statement and decision obligations
   end record;

   --  Both Witness subprograms below set boolean corresponding to Bit to True
   --  in Buffer. The function form is used to set booleans in the middle of
   --  declarative parts.

   procedure Witness (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id);

   type Witness_Dummy_Type is null record;
   function Witness
     (Buffer : in out Coverage_Buffer_Type; Bit : Bit_Id)
      return Witness_Dummy_Type;

   function Witness
     (Buffer              : in out Coverage_Buffer_Type;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean;
   --  If Value is false, set the Boolean corresponding to False_Bit to True in
   --  Buffer. Set the one corresponding to True_Bit otherwise.

end System.GNATcov.Buffers;
