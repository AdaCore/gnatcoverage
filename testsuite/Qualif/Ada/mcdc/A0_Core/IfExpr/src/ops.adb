package body Ops is
   function Eval (Op : Op_Not_T; B : Boolean) return Boolean is
   begin
      return not B;
   end;

   function Eval (Op : Op_Self_T; B : Boolean) return Boolean is
   begin
      return B;
   end;

   function Eval (Op : Op_And_T; A, B : Boolean) return Boolean is
   begin
      return A and then B;
   end;
end;
