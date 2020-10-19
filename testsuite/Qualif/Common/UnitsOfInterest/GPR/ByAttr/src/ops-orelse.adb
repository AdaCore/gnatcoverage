package body Ops.Orelse is
   function Eval (A, B : Boolean) return Boolean is
   begin
      return A or else B; -- # eval
   end;
end;
