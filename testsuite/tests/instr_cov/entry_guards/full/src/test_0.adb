with Pkg;
with Support; use Support;

procedure Test_0 is
begin
   Assert (True);
end Test_0;

--# pkg.ads
-- /init/  l- ## s-
--
--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/  l- ## s-
-- =/guard/ l. ## 0
-- =/stmt/  l- ## s-
--
-- %cov: --level=stmt\+decision
-- =/init/  l- ## s-
-- =/guard/ l- ## d-
-- =/stmt/  l- ## s-
--
-- %cov: --level=.*mcdc
-- =/init/  l- ## s-
-- =/guard/ l- ## d-
-- =/stmt/  l- ## s-
