package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Aop : Capsum renames Ops.A; -- # decl
      Bop : Capsum renames Ops.B; -- # decl
   begin
      if Ops.A.X + Aop.Y <= Aop.Cap -- # eval0
        and then Ops.B.X + Bop.Y <= Bop.Cap -- # eval1
      then
         return True; -- # true
      else
         return False; -- # false
      end if;
   end;
end;
