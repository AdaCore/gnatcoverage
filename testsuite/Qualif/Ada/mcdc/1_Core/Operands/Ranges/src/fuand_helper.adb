with FUAND, Sensors, Support; use FUAND, Sensors, Support;

package body FUAND_Helper is

   procedure Eval_FF_F is
   begin
      Assert (Both_In_Range ((A => NOK, B => NOK)) = False);
   end;

   procedure Eval_FT_F is
   begin
      Assert (Both_In_Range ((A => NOK, B => OK)) = False);
   end;

   procedure Eval_TF_F is
   begin
      Assert (Both_In_Range ((A => OK, B => NOK)) = False);
   end;

   procedure Eval_TT_T is
   begin
      Assert (Both_In_Range ((A => OK, B => OK)) = True);
   end;

end;

