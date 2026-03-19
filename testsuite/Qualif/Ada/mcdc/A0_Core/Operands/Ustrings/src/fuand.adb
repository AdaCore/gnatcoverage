
package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
      Aop : Caller_Operand renames Ops.A; -- # decl
      Bop : Caller_Operand renames Ops.B; -- # decl
   begin
      return Operand'(Aop.X > 0, Aop.US) = (True, To_Unbounded_String("USA")) -- # evalA
        and then Operand'(Bop.X > 0, Bop.US) = (True, To_Unbounded_String("USB")); -- # evalB
   end;
end;
