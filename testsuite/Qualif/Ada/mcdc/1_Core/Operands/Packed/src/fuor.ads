package FUOR is
   type I8 is new Integer range -2 ** 7 .. 2 * 7 -1;
   for I8'Size use 8;
   
   type Operands is record
      Sand : Boolean;
      A, B : I8;
   end record;
   pragma Pack (Operands);
   
   function Orelse (Ops : Operands) return Boolean;
end;
