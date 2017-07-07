package FUOR is
   
   type T_Values is (TT, FF);
   for T_Values use (4, 5);
   
   type Operands is record
      A, B : T_Values;
   end record;
   
   function Orelse (Ops : Operands) return Boolean;
end;
