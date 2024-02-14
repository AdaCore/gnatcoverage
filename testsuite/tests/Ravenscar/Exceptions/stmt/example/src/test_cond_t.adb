with Support, Cond_Raise; use Support;

procedure Test_Cond_T is
begin
   Cond_Raise.Check_For (Cond => True);

   --  We called once only, raising. So ...

   --  We raised and went into the exception handler once

   Assert (Cond_Raise.N_Raise = 1);
   Assert (Cond_Raise.N_In_Handler = 1);

   --  We never reached the code past the test deciding if we should raise
   --  (which we only reach when we call the function without raising).
   --  Likewise for the code past the inner call, skipped by the exception
   --  propagation.

   Assert (Cond_Raise.N_Past_Test = 0);
   Assert (Cond_Raise.N_Past_Call = 0);

end;

--  Expectations on =xcov and =report coverage outcome follow. Note the single
--  reference to the "raise" marker to designate multiple lines/statements in
--  the Raise_If suprogram, for which we indeed always expect the same kind of
--  outcome (all the lines/statements are expected to be covered/uncovered in
--  a consistent fashion).

--# cond_raise.adb
-- /test_cond/  l+ ## 0
-- /raise/      l+ ## 0
-- /past_test/  l- ## s-
-- /call/       l+ ## 0
-- /past_call/  l- ## s-
-- /in_handler/ l+ ## 0
