with FUOR, Support; use FUOR, Support;

package body FUOR_Helper is

   M0 : Modop := (X => 15, Y => 5);
   M1 : Modop := (X => 16, Y => 5);

   procedure Eval_FF_F is
   begin
      Assert (Mod0_Or (OpA => M1, OpB => M1) = False);
   end;

   procedure Eval_FT_T is
   begin
      Assert (Mod0_Or (OpA => M1, OpB => M0) = True);
   end;

   procedure Eval_TF_T is
   begin
      Assert (Mod0_Or (OpA => M0, OpB => M1) = True);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Mod0_Or (OpA => M0, OpB => M0) = True);
   end;

end;

