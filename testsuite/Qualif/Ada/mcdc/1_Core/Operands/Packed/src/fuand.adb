package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      return Ops.A > 0 -- # evalA
        and then Ops.B > 0; -- # evalB
   end;
end;
