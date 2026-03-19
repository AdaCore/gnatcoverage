with Support, Fuor; use Fuor, Support;

package body FUOR_Helper is

   V : Boolean;

   procedure Eval_FF_F is
   begin
      Fuor.Orelse (F, F, V);
      Assert (V = False);
   end;

   procedure Eval_TX_T is
   begin
      Fuor.Orelse (T, F, V);
      Assert (V  = True);
      Fuor.Orelse (T, T, V);
      Assert (V  = True);
   end;

   procedure Eval_FT_T is
   begin
      Fuor.Orelse (F, T, V);
      Assert (V = True);
   end;

   procedure Eval_RX is
   begin
      Fuor.Orelse (R, F, V);
   end;

   procedure Eval_FR is
   begin
      Fuor.Orelse (F, R, V);
   end;
end;
