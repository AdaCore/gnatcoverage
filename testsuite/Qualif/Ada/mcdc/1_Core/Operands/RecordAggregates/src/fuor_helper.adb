with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Orelse ((FFA, FFB)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Orelse ((FFA, TTB)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Orelse ((TTA, FFB)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Orelse ((TTA, TTB)));
   end;

end;

