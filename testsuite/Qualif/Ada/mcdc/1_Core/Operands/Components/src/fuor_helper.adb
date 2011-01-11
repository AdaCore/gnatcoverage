with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Orelse ((A => False, B => False)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Orelse ((A => False, B => True)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Orelse ((A => True, B => False)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Orelse ((A => True, B => True)));
   end;

end;

