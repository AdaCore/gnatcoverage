package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      return Ops.A -- # evalA
        or else Ops.B; -- # evalB
   end;
end;
