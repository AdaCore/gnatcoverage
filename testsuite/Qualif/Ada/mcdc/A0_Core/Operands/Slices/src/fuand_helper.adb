with FUAND, Blocks, Support; use FUAND, Blocks, Support;

package body FUAND_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Match_P_And_S (Sample, Nopre, Nosuf) = False);
   end;

   procedure Eval_FT_F is
   begin
      Assert (Match_P_And_S (Sample, Nopre, Suf) = False);
   end;

   procedure Eval_TF_F is
   begin
      Assert (Match_P_And_S (Sample, Pre, Nosuf) = False);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Match_P_And_S (Sample, Pre, Suf) = True);
   end;

end;
