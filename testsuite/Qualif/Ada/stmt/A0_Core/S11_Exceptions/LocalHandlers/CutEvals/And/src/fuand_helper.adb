with Support, Fuand; use Fuand, Support;

package body FUAND_Helper is

   V : Boolean;

   procedure Eval_FX_F is
   begin
      Fuand.Andthen (F, T, V);
      Assert (V = False);
      Fuand.Andthen (F, F, V);
      Assert (V = False);
   end;

   procedure Eval_TF_F is
   begin
      Fuand.Andthen (T, F, V);
      Assert (V = False);
   end;

   procedure Eval_TT_T is
   begin
      Fuand.Andthen (T, T, V);
      Assert (V = True);
   end;

   procedure Eval_RX is
   begin
      Fuand.Andthen (R, F, V);
   end;

   procedure Eval_TR is
   begin
      Fuand.Andthen (T, R, V);
   end;
end;
