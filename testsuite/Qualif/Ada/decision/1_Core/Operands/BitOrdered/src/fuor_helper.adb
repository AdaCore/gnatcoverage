with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Orelse (Ops_FF_F));
   end;

   procedure Eval_FT_T is
   begin
      Assert (Orelse (Ops_FT_T));
   end;

   procedure Eval_TX_T is
   begin
      Assert (Orelse (Ops_TF_T));
      Assert (Orelse (Ops_TT_T));
   end;

end;

