with Evals; use Evals;

package body A1O2 is

   procedure Process (A, B, C, D : Boolean) is
   begin
      Eval (A and then B, C or else D); -- # evals
   end;
end;

