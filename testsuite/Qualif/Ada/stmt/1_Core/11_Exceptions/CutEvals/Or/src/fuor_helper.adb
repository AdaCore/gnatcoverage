with Support, Fuor; use Fuor, Support;

package body FUOR_Helper is

   V : Boolean;

   procedure Eval_FF_F is
   begin
      Assert (Fuor.Orelse (F, F) = False);
   end;

   procedure Eval_TX_T is
   begin
      Assert (Fuor.Orelse (T, F) = True);
      Assert (Fuor.Orelse (T, T) = True);
   end;

   procedure Eval_FT_T is
   begin
      Assert (Fuor.Orelse (F, T) = True);
   end;

   procedure Eval_RX is
   begin
      V := Fuor.Orelse (R, F);
   end;

   procedure Eval_FR is
   begin
      V := Fuor.Orelse (F, R);
   end;
end;
