with Pkg;
with Support; use Support;

procedure Test_0 is
begin
   Assert (True);
end Test_0;

--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/        l- ## s-
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l. ## 0
-- =/wait-stmts/  l- ## s-
--
-- %cov: --level=stmt\+decision
-- =/init/        l- ## s-
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l- ## d-
-- =/wait-stmts/  l- ## s-
--
-- %cov: --level=.*mcdc
-- =/init/        l- ## s-
-- =/set-a-stmts/ l- ## s-
-- =/set-b-stmts/ l- ## s-
-- =/wait-guard/  l- ## d-
-- =/wait-stmts/  l- ## s-
