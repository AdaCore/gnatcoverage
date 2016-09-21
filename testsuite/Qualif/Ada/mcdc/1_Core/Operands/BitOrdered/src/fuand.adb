package body FUAND is

   function Andthen (Ops : Operands) return Boolean is
   begin
      return Ops.A -- # evalA
        and then Ops.B; -- # evalB
   end;
end;
