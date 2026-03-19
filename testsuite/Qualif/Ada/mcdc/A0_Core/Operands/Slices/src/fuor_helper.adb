with FUOR, Blocks, Support; use FUOR, Blocks, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Match_P_Or_S (Sample, Nopre, Nosuf) = False);
   end;

   procedure Eval_FT_T is
   begin
      Assert (Match_P_Or_S (Sample, Nopre, Suf) = True);
   end;

   procedure Eval_TF_T is
   begin
      Assert (Match_P_Or_S (Sample, Pre, Nosuf) = True);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Match_P_Or_S (Sample, Pre, Suf) = True);
   end;

end;
