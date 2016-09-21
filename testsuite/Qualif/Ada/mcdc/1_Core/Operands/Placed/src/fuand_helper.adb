with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is
   
   procedure Eval_FF_F is
   begin
      Assert (not Andthen ((Sand => True, A => 0, B => 0)));
   end;

   procedure Eval_FT_F is
   begin
      Assert (not Andthen ((Sand => True, A => 0, B => 1)));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Andthen ((Sand => True, A => 1, B => 0)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Andthen ((Sand => True, A => 1, B => 1)));
   end;

end;

