with Support, Val; use Support, Val;

package body Val_Helper is

   V : Boolean;

   procedure Eval_T is
   begin
      Bool (T, V);
      Assert (V = True);
   end;
   procedure Eval_F is
   begin
      Bool (F, V);
      Assert (V = False);
   end;
   procedure Eval_R is
   begin
      Bool (R, V);
   end;
end;
