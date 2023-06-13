with Sensors; use Sensors;
procedure Test_CNFO is
begin
   --  F out of range

   Sensors.Test (0.0, F);
end Test_CNFO;

--# sensors.adb
-- /check-test-c/ s=>l+, dmu=>l! ## s=>0, dmu=>dT-
-- /check-do-c/   l- ## s-
-- /check-do-f/   l+ ## 0

-- %cov: -S instance
-- Not a generic body, so no possible instance tag.

-- %cov: -S routine
-- =/check-test-c/ s=>l+, dmu=>l! ## s=>0, dmu=>dT-@(sensors__test)
-- =/check-do-c/  l- ## s-@(sensors__test)

--# sensors.ads
-- No expectation for this unit. Reference is needed here, still, to
-- get visibility on generic instantiations in this source.

--# check_g.adb
--  /test-out/   s=>l+, dmu=>l! ## s=>0, dmu=>dF-
--  /in-range/   l- ## s-
--  /out-range/  l+ ## 0

-- %cov: -S instance
--  =/test-out/ l! ## s-@(i:RC), dF-@(i:RF)
--  =/in-range/   l- ## s-@(i:RF), s-@(i:RC)
--  =/out-range/  l! ## s-@(i:RC)

-- %cov: -S routine %cargs: !-gnatn
--  =/test-out/  l! ## s-@(sensors__rc__test), dF-@(sensors__rf__test)
--  =/in-range/  l- ## s-@(sensors__rf__test), s-@(sensors__rc__test)
--  =/out-range/  l! ## s-@(sensors__rc__test)


