package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
      Aop : Capsum renames Ops.A; -- # decl
      Bop : Capsum renames Ops.B; -- # decl
   begin
      return Ops.A.X + Aop.Y <= Aop.Cap -- # evalA
        or else Ops.B.X + Bop.Y <= Bop.Cap; -- # evalB
   end;

end;
