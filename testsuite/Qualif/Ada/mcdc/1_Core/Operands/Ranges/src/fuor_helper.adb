with FUOR, Sensors, Support; use FUOR, Sensors, Support;

package body FUOR_Helper is

   procedure Eval_FF_F is
   begin
      Assert (One_In_Range ((A => NOK, B => NOK)) = False);
   end;

   procedure Eval_FT_T is
   begin
      Assert (One_In_Range ((A => NOK, B => OK)) = True);
   end;

   procedure Eval_TF_T is
   begin
      Assert (One_In_Range ((A => OK, B => NOK)) = True);
   end;

   procedure Eval_TT_T is
   begin
      Assert (One_In_Range ((A => OK, B => OK)) = True);
   end;

end;

