with Evals; use Evals;

package body A1O1 is

   procedure Process (A, B : Boolean) is
   begin
      --  mutliple (and different) decisions over the same arguments on the
      --  same line below:
      Eval (A and then B, A or else B); -- # evals
   end;
end;
