with Support, Fuand; use Fuand, Support;

package body FUAND_Helper is

   procedure Eval_FX_F is
   begin
      Assert (Fuand.Eval (R1, R2_FX) = False);
   end;
   
   procedure Eval_TF_F is
   begin
      Assert (Fuand.Eval (R1, R2_TF) = False);		     
   end;
   
   procedure Eval_TT_T is
   begin
      Assert (Fuand.Eval (R1, R2_TT) = True);		     
   end;

end;
