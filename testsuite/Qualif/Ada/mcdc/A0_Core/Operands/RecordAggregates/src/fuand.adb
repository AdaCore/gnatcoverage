package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Aop : Caller_Operand renames Ops.A; -- # decl
      Bop : Caller_Operand renames Ops.B; -- # decl
   begin
      return Operand'(Aop.X > 0, (Aop.Y mod 2) = 0 , Aop.K) = (True, True, "AAKEY") -- # evalA
        and then Operand'(Bop.X > 0, (Bop.Y mod 2) = 0, Bop.K) = (True, False, "BBKEY"); -- # evalB
   end;
end;
