package body MultibackProp is
   procedure Q is
      X : Integer;
      pragma Volatile (X);
   begin
      X := 12; -- # trueEval
   end Q;

   procedure P (X : Boolean) is
   begin
      if X then                 -- # alwaysEval
         pragma Annotate (Nop); -- # trueEval
         pragma Annotate (Nop); -- # trueEval
         Q;                     -- # trueEval
      end if;
   end P;
end MultibackProp;
