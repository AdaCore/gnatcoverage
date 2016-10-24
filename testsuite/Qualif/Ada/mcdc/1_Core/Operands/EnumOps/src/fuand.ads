package FUAND is
   
   type Value_T is (TT, FF);
   
   type Operands is record
      A, B : Value_T;
   end record;
   
   function Andthen (Ops : Operands) return Boolean;
end;
