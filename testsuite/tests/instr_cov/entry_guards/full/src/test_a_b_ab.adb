with Pkg;

procedure Test_A_B_AB is
   T : Pkg.PT;
begin
   T.Init (A => True, B => False);
   select
      T.Wait_Cond;
   else
      null;
   end select;

   T.Init (A => False, B => True);
   select
      T.Wait_Cond;
   else
      null;
   end select;

   T.Init (A => True, B => True);
   T.Wait_Cond;
end Test_A_B_AB;

--# pkg.ads
-- /init/  l+ ## 0
--
--# pkg.adb
--
-- %cov: --level=stmt
-- =/init/  l+ ## 0
-- =/guard/ l. ## 0
-- =/stmt/  l+ ## 0
--
-- %cov: --level=stmt\+decision
-- =/init/  l+ ## 0
-- =/guard/ l+ ## 0
-- =/stmt/  l+ ## 0
--
-- %cov: --level=.*mcdc
-- =/init/  l+ ## 0
-- =/guard/ l+ ## 0
-- =/stmt/  l+ ## 0
