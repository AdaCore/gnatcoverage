with Evals; use Evals;

package body A1A1 is

   procedure Process (A, B : Boolean) is
   begin
      --  distinct decisions over the same arguments on the
      --  same line below:
      Eval (A and then B, A and then B); -- # evals
   end;
end;
