package body MultibackProp is

   procedure Q is
      --  Dummy subprogram called from P to provide a backpropagation
      --  start anchor. Make sure there is code associated with it, to
      --  prevent the call from being just not emitted at all.

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
