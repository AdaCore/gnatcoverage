package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Aop : Operand renames Ops.A; -- # decl
      Bop : Operand renames Ops.B; -- # decl
   begin
      return Ops.A.Data (Aop.Index+1) = 'T' -- # evalA
        and then Ops.B.Data (Bop.Index+1) = 'T'; -- # evalB
   end;
end;
