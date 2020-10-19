package body Boolops.Andthen is
   function Eval (A, B : Boolean) return Boolean is
   begin
      return A and then B; -- # eval
   end;
end;
