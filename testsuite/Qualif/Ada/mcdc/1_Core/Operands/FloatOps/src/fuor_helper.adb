with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Orelse ((FF, FF)));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Orelse ((FF, TT)));
   end;

   procedure Eval_TF_T is
   begin
      Assert (Orelse ((TT, FF)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Orelse ((TT, TT)));
   end;

end;

