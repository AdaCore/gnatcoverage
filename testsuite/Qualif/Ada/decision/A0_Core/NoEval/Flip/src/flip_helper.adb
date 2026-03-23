with Support, Flip; use Support, Flip;

package body Flip_Helper is

   procedure Eval_T is
   begin
      Assert (Neg (T) = False);
   end;
   procedure Eval_F is
   begin
      Assert (Neg (F) = True);
   end;
   procedure Eval_R is
      V : Boolean;
   begin
      V := Neg (R);
   end;
end;
