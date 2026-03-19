with Pkg;

procedure Test_A is
   T : Pkg.PT;
begin
   T.Init (A => True, B => False);
   select
      T.Wait_Cond;
   else
      null;
   end select;
end Test_A;

--# pkg.ads
-- /init/  l+ ## 0
--
--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/  l+ ## 0
-- =/guard/ l. ## 0
-- =/stmt/  l- ## s-
--
-- %cov: --level=stmt\+decision
-- =/init/  l+ ## 0
-- =/guard/ l! ## dT-
-- =/stmt/  l- ## s-
--
-- %cov: --level=.*mcdc
-- =/init/  l+ ## 0
-- =/guard/ l! ## dT-
-- =/stmt/  l- ## s-
