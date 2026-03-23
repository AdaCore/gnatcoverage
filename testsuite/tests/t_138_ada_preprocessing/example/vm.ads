package VM is

   type PC_Type is new Positive;
   type SP_Type is new Positive;

   type Opcode is (Halt, Jump, Branch, Push_Lit, Clone, Add);
   type Instruction_Type (Kind : Opcode := Opcode'First) is record
      case Kind is
         when Halt          => null;
         when Jump | Branch => Jump_Dest : PC_Type;
         when Push_Lit      => Push_Value : Integer;
         when Clone | Add   => null;
      end case;
   end record;

   type Stack_Type is array (SP_Type range <>) of Integer;
   type Program_Type is array (PC_Type range <>) of Instruction_Type;

   procedure Eval
     (Program : Program_Type;
      PC      : in out PC_Type;
      Stack   : in out Stack_type;
      SP      : in out SP_Type);

   function Eval
     (Program        : Program_Type;
      Stack_Size     : Natural;
      Initial_Values : Stack_Type) return Integer;

end VM;
