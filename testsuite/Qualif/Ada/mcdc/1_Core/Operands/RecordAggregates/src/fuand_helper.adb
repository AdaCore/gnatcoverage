with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is
   
   procedure Eval_FF_F is
   begin
      Assert (not Andthen ((FFA, FFB)));
   end;

   procedure Eval_FT_F is
   begin
      Assert (not Andthen ((FFA, TTB)));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Andthen ((TTA, FFB)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Andthen ((TTA, TTB)));
   end;

end;

