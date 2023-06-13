package Cond_Raise is
   procedure Check_For (Cond : Boolean);

   N_Past_Call, N_In_Handler, N_Raise, N_Past_Test : Natural := 0;
end;
