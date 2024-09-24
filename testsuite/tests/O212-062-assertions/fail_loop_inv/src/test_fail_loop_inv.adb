with Fail_Loop_Inv;

--  Test the coverage state of a failed loop invariant.

procedure Test_Fail_Loop_Inv is
begin
   Fail_Loop_Inv;
end Test_Fail_Loop_Inv;

--# fail_loop_inv.adb
-- /high_decl/ l+ ## 0
-- /loop_cond/ l+ ## 0
-- /loop_inv/  l! ## aT-
-- /catch/     l+ ## 0
