with System.Machine_Code; use System.Machine_Code;

package body MultibackProp is

   procedure Q is
      --  Dummy subprogram called from P to provide a backpropagation
      --  start anchor. Make sure there is code associated with it, to
      --  prevent the call from being just not emitted at all.

      X : Integer;         -- # truedecl
      pragma Volatile (X); -- # truedecl
   begin
      X := 12; -- # trueEval
   end Q;

   procedure P (X : Boolean) is
   begin
      if X then                  -- # alwaysEval

         --  The following Asm calls only insert comments in the generated
         --  assembly code, no executable code at all.

         Asm ("# nop", Volatile => True); -- # trueEval
         Asm ("# nop", Volatile => True); -- # trueEval

         Q;                               -- # trueEval
      end if;
   end P;
end MultibackProp;
