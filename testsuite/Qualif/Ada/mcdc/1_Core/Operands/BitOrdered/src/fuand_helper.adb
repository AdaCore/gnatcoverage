with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is

   procedure Eval_FF_F is
   begin
      Assert (not Andthen ((A => False, B => False)));
   end;

   procedure Eval_FT_F is
   begin
      Assert (not Andthen ((A => False, B => True)));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Andthen ((A => True, B => False)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Andthen ((A => True, B => True)));
   end;

end;
