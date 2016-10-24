with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is
   
   procedure Eval_FF_F is
   begin
      Assert (not Andthen ((FF, FF)));
   end;

   procedure Eval_FT_F is
   begin
      Assert (not Andthen ((FF, TT)));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Andthen ((TT, FF)));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Andthen ((TT, TT)));
   end;

end;

