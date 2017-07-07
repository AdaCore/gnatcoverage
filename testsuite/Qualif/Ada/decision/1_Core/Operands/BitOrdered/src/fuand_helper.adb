with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is

   procedure Eval_FX_F is
   begin
      Assert (not Andthen (Ops_FF_F));
      Assert (not Andthen (Ops_FT_F));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Andthen (Ops_TF_F));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Andthen (Ops_TT_T));
   end;

end;

