with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Orelse ((A => 0.0, B => 0.0)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Orelse ((A => 0.0, B => 1.0)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Orelse ((A => 1.0, B => 0.0)));
   end;

   procedure Eval_TX_T is
   begin
      Assert (Orelse ((A => 1.0, B => 1.0)));
   end;

end;

