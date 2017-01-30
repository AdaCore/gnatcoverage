with System; use System;

package FUAND is
   
   type Operands is record
      A, B : Boolean;
   end record;
   
   for Operands'Bit_Order use High_Order_First;
   for Operands use record
      A at 0 range 0 .. 0;
      B at 0 range 1 .. 1;
   end record;
   
   function Andthen (Ops : Operands) return Boolean;
end;
