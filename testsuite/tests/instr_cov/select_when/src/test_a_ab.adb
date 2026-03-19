with Pkg;

procedure Test_A_AB is
   T : Pkg.T;
begin
   T.Init (A => True, B => False);

   select
      T.Wait_Cond;  -- Guard: True and False => False
   else
      null;
   end select;

   T.Set_B (True);
   T.Wait_Cond;  --   Guard: True and True => True
end Test_A_AB;

--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l+ ## 0
-- =/wait-guard/  l. ## 0
-- =/wait-stmts/  l+ ## 0
--
-- %cov: --level=stmt\+decision
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l+ ## 0
-- =/wait-guard/  l+ ## 0
-- =/wait-stmts/  l+ ## 0
--
-- %cov: --level=.*mcdc
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l+ ## 0
-- =/wait-guard/  l! ## c!:"A"
-- =/wait-stmts/  l+ ## 0
