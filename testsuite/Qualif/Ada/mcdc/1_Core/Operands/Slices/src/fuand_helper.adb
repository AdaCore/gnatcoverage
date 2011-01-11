with FUAND, Support; use FUAND, Support;

package body FUAND_Helper is

   Sample : Block := "1234567890";

   Pre   : Prefix := "123";
   Nopre : Prefix := "xxx";
   Suf   : Suffix := "890";
   Nosuf : Suffix := "xxx";

   procedure Eval_FF_F is
   begin
      Assert (not Match (Sample, Nopre, Nosuf));
   end;

   procedure Eval_FT_F is
   begin
      Assert (not Match (Sample, Nopre, Suf));
   end;

   procedure Eval_TF_F is
   begin
      Assert (not Match (Sample, Pre, Nosuf));
   end;

   procedure Eval_TT_T is
   begin
      Assert (Match (Sample, Pre, Suf));
   end;

end;
