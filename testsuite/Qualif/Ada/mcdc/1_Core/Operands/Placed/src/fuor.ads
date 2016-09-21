package FUOR is
   type I32 is new Integer range -2 ** 31 .. 2 * 31 -1;
   for I32'Size use 32;
   
   type Operands is record
      Sand : Boolean;
      A, B : I32;
   end record;
   for Operands use record
      Sand at 0 range 0 .. 0;
      A at 0 range 1 .. 32;
      B at 0 range 33 .. 64;
   end record;
   
   function Orelse (Ops : Operands) return Boolean;
end;
