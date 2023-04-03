with Support, Flip; use Support, Flip;

package body Flip_Helper is

   V : Boolean;

   procedure Eval_T is
   begin
      Neg (T, V);
      Assert (V = False);
   end;
   procedure Eval_F is
   begin
      Neg (F, V);
      Assert (V = True);
   end;
   procedure Eval_R is
   begin
      Neg (R, V);
   end;
end;
