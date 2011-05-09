with Support, Fuor; use Support, Fuor;

package body Fuor_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Fuor.Eval (R1, R2_FF) = False);
   end;

   procedure Eval_FT_T is
   begin
      Assert (Fuor.Eval (R1, R2_FT) = True);
   end;

   procedure Eval_TX_T is
   begin
      Assert (Fuor.Eval (R1, R2_TX) = True);
   end;

end;
