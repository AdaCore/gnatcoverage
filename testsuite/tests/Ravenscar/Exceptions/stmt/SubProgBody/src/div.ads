package Div is

   N_In_Handler : Natural := 0;
   My_Constraint_Error: exception;
   N_In_MyCE_Handler: Natural := 0;
   N_In_Wrong_Handler: Natural := 0;

   procedure Divide (X, Y: Integer; Res: out Integer);

   N_Comp_Success : Integer := 0;
   N_Excpt_Prop : Integer := 0;
   N_Wrong_Excpt_Prop : Integer := 0;

end Div;
