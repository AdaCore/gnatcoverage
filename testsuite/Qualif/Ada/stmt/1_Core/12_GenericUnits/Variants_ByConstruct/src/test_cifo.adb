with Sensors, Support; use Sensors, Support;

procedure Test_CIFO is
begin
   --  C in range

  Sensors.Test (37.0, C);

   --  F out of range

  Sensors.Test (0.0, F);

  Assert (Sensors.N_Tests_C = 1);
  Assert (Sensors.N_Tests_F = 1);
  
  Assert (RC.In_Range_Count = 1);
  Assert (RC.Out_Of_Range_Count = 0);
  Assert (RF.In_Range_Count = 0);
  Assert (RF.Out_Of_Range_Count = 1);
  
end Test_CIFO;

--# sensors.adb
-- /check-test-c/ l+ ## 0
-- /check-do-c/   l+ ## 0
-- /check-do-f/   l+ ## 0

-- %cov: -S instance
-- Not a generic body, so no possible instance tag.

--# sensors.ads
-- No expectation for this unit. Reference is needed here, still, to
-- get visibility on generic instantiations in this source.

--# check_g.adb
-- defaults, for instances conslidated:
--  /test-out/   l+ ## 0
--  /in-range/   l+ ## 0
--  /out-range/  l+ ## 0

-- %cov: -S instance
--  =/test-out/   s=>l+, dmu=>l! ## dT-@(i:RC), dF-@(i:RF)
--  =/in-range/   l! ## s-@(i:RF)
--  =/out-range/  l! ## s-@(i:RC)
