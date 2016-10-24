package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      return Ops.A = TT -- # evalA
        and then Ops.B = TT; -- # evalB
   end;
end;
