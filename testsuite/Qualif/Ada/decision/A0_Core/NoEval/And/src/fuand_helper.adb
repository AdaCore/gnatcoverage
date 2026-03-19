with Support, Fuand; use Fuand, Support;

package body FUAND_Helper is

   V : Boolean;

   procedure Eval_FX_F is
   begin
      Assert (Fuand.Andthen (F, T) = False);
      Assert (Fuand.Andthen (F, F) = False);
   end;

   procedure Eval_TF_F is
   begin
      Assert (Fuand.Andthen (T, F) = False);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Fuand.Andthen (T, T) = True);
   end;

   procedure Eval_RX is
   begin
      V := Fuand.Andthen (R, F);
   end;

   procedure Eval_TR is
   begin
      V := Fuand.Andthen (T, R);
   end;
end;
