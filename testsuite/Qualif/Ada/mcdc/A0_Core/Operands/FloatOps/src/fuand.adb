package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Aop : Capsum renames Ops.A; -- # decl
      Bop : Capsum renames Ops.B; -- # decl
   begin
      return Ops.A.X + Aop.Y <= Aop.Cap -- # evalA
        and then Ops.B.X + Bop.Y <= Bop.Cap; -- # evalB
   end;
end;
