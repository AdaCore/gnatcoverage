with Support, Cond_Raise; use Support;

procedure Test_Cond_F is
begin
   Cond_Raise.Check_For (Cond => False);
   Assert (Cond_Raise.N_Raise = 0);
   Assert (Cond_Raise.N_Past_Test = 1);
   Assert (Cond_Raise.N_In_Handler = 0);
   Assert (Cond_Raise.N_Past_Call = 1);
end;

--# cond_raise.adb
-- /test_cond/ l+ ## 0
-- /raise/ l- ## s-
-- /past_test/ l+ ## 0
-- /call/ l+ ## 0
-- /past_call/ l+ ## 0
-- /in_handler/ l- ## s-
