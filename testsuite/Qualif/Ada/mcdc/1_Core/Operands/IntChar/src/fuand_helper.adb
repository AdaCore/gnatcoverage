with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is
   
   procedure Eval_FF_F is
   begin
      Assert (not Andthen ((FF1, FF2)));
   end;

   procedure Eval_FT_F is
   begin
      Assert (not Andthen ((FF1, TT1)));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Andthen ((TT2, FF1)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Andthen ((TT1, TT2)));
   end;

end;

