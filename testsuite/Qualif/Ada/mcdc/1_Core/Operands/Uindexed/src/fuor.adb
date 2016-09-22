package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      return Ops(A) > 0.0 -- # evalA
        or else Ops(B) > 0.0; -- # evalB
   end;
end;
