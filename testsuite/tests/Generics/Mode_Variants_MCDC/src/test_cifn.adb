with Support, Sensors; use Support, Sensors;

procedure Test_CIFN is
begin
   --  C in range

  Sensors.Test (37.0, C);

  Assert (Sensors.N_Tests_C = 1);
  Assert (Sensors.N_Tests_F = 0);

  Assert (RC.In_Range_Count = 1);
  Assert (RC.Out_Of_Range_Count = 0);
  Assert (RF.In_Range_Count = 0);
  Assert (RF.Out_Of_Range_Count = 0);
end Test_CIFN;

--# sensors.adb
-- /check-test-c/ s=>l+, dmu=>l! ## s=>0, dmu=>dF-
-- /check-do-c/   l+ ## 0
-- /check-do-f/   l- ## s-

-- %cov: -S instance
-- Not a generic body, so no possible instance tag.

-- %cov: -S routine
-- =/check-test-c/ s=>l+, dmu=>l! ## s=>0, dmu=>dF-@(sensors__test)
-- =/check-do-f/  l- ## s-@(sensors__test)

--# sensors.ads
-- No expectation for this unit. Reference is needed here, still, to
-- get visibility on generic instantiations in this source.

--# check_g.adb
--  /test-out/   s=>l+, dmu=>l! ## s=>0, dmu=>dT-
--  /in-range/   l+ ## 0
--  /out-range/  l- ## s-

-- %cov: -S instance
--  =/test-out/ l! ## s-@(i:RF), dT-@(i:RC)
--  =/in-range/   l! ## s-@(i:RF)
--  =/out-range/  l- ## s-@(i:RF), s-@(i:RC)

-- %cov: -S routine %cargs: !-gnatn
--  =/test-out/  l! ## s-@(sensors__rf__test), dT-@(sensors__rc__test)
--  =/in-range/  l! ## s-@(sensors__rf__test)
--  =/out-range/  l- ## s-@(sensors__rf__test), s-@(sensors__rc__test)
