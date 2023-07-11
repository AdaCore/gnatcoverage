with Pkg;

procedure Test_B_AB is
   T : Pkg.T;
begin
   T.Init (A => False, B => True);

   select
      T.Wait_Cond;  -- Guard: False and True => False
   else
      null;
   end select;

   T.Set_A (True);
   T.Wait_Cond;  --   Guard: True and True => True
end Test_B_AB;

--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l+ ## 0
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l. ## 0
-- =/wait-stmts/  l+ ## 0
--
-- %cov: --level=stmt\+decision
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l+ ## 0
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l+ ## 0
-- =/wait-stmts/  l+ ## 0
--
-- %cov: --level=.*mcdc
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l+ ## 0
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l! ## c!:"B"
-- =/wait-stmts/  l+ ## 0
