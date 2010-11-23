package body MultibackProp is
   procedure Q is
   begin
      null; -- # trueEval
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
