with Evals; use Evals;

package body A1A2 is
   
   procedure Process (A, B, C, D : Boolean) is
   begin
      Eval (A and then B, C and then D); -- # evals
   end;
end;
  
