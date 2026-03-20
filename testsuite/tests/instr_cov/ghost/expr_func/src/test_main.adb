pragma Ada_2012;

with Pkg;

procedure Test_Main is
begin
   Pkg.Say_Even;
end Test_Main;

--# pkg.adb
-- /say-even/  l+ ## 0
--
--# pkg.ads
-- /is-even/   l. ## 0
--%opts:--instrument-ghost
-- =/is-even/  l! ## eF-
