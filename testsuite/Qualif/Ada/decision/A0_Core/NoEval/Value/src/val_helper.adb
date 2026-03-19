with Support, Val; use Support, Val;

package body Val_Helper is

   procedure Eval_T is
   begin
      Assert (Bool (T) = True);
   end;
   procedure Eval_F is
   begin
      Assert (Bool (F) = False);
   end;
   procedure Eval_R is
      V : Boolean;
   begin
      V := Bool (R);
   end;
end;
