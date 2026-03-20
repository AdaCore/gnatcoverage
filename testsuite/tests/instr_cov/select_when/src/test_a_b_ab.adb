with Pkg;

procedure Test_A_B_AB is
   T : Pkg.T;
begin
   T.Init (A => False, B => False);

   T.Set_A (True);
   select
      T.Wait_Cond;  -- Guard: True and False => False
   else
      null;
   end select;

   T.Set_A (False);
   T.Set_B (True);
   select
      T.Wait_Cond;  -- Guard: False and True => False
   else
      null;
   end select;

   T.Set_A (True);
   T.Wait_Cond;  --   Guard: True and True => True
end Test_A_B_AB;

--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l+ ## 0
-- =/set-b-stmts/ l+ ## 0
-- =/wait-guard/  l. ## 0
-- =/wait-stmts/  l+ ## 0
--
-- %cov: --level=stmt\+decision
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l+ ## 0
-- =/set-b-stmts/ l+ ## 0
-- =/wait-guard/  l+ ## 0
-- =/wait-stmts/  l+ ## 0
--
-- %cov: --level=.*mcdc
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l+ ## 0
-- =/set-b-stmts/ l+ ## 0
-- =/wait-guard/  l+ ## 0
-- =/wait-stmts/  l+ ## 0
