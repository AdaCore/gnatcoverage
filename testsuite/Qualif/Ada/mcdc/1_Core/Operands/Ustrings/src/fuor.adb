with Ada.Strings.Maps; use Ada.Strings.Maps;

package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
      Aop : Caller_Operand renames Ops.A; -- # decl
      Bop : Caller_Operand renames Ops.B; -- # decl
      
   begin
      return OperandA'(Match => To_String(Aop.US1 & Aop.US2) = "STARTEND") = (Match => True) -- # evalA
        or else OperandB'(To_Match => Translate (Bop.US1, To_Mapping("ABC", "012"))) = (To_Match => Bop.US2); -- # evalB
   end;

end;
