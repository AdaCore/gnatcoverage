with Support, Fubool; use Fubool, Support;

package body FUBOOL_Helper is

   procedure Eval_F is
   begin
      Assert (Fubool.Eval (R1, R2_F) = False);
   end;
      
   procedure Eval_T is
   begin
      Assert (Fubool.Eval (R1, R2_T) = True);
   end;

end;
