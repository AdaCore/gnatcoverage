with Pkg;

procedure Test_False is
   T : Pkg.T;
begin
   T.Init (A => False, B => False);

   select
      T.Wait_Cond;  -- Guard: False and False => False
   else
      null;
   end select;
end Test_False;

--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l. ## 0
-- =/wait-stmts/  l- ## s-
--
-- %cov: --level=stmt\+decision
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l! ## dT-
-- =/wait-stmts/  l- ## s-
--
-- %cov: --level=.*mcdc
-- =/init/        l+ ## 0
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l! ## dT-
-- =/wait-stmts/  l- ## s-
