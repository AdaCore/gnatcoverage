with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Orelse ((FF1, FF2)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Orelse ((FF1, TT1)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Orelse ((TT2, FF2)));
   end;

   procedure Eval_TX_T is
   begin
      Assert (Orelse ((TT1, FF2)));
   end;

end;

