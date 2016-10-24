package body FUOR is

   function Orelse (Ops : Operands) return Boolean is
   begin
      return Ops.A = TT -- # evalA
        or else Ops.B = TT; -- # evalB
   end;

end;
