package FUOR is
   type Operands is record
      A, B : Boolean;
   end record;

   function Orelse (Ops : Operands) return Boolean;
end;
