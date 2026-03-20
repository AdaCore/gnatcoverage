package FUAND is
   type Operands is record
      A, B : Boolean;
   end record;

   function Andthen (Ops : Operands) return Boolean;
end;
