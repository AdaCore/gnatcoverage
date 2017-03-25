with Sensors, Support; use Sensors, Support;

-- CBFN : Invoking Both in and out of range for unit "C". None for unit "F".

procedure Test_CBFN is
begin
   --  C in range (!low !high => Out-False)

  Sensors.Test (37.0, C);
            
   --  C out of range (!low, high => Out-True)

  Sensors.Test (200.0, C);
  
  Assert (Sensors.N_Tests_C = 2);
  Assert (Sensors.N_Tests_F = 0);
  
  Assert (RC.In_Range_Count = 1);
  Assert (RC.Out_Of_Range_Count = 1);
  Assert (RF.In_Range_Count = 0);
  Assert (RF.Out_Of_Range_Count = 0);
  
end Test_CBFN;

--# sensors.adb
-- /check-test-c/ l+ ## 0
-- /check-do-c/   l+ ## 0
-- /check-do-f/   l- ## s-

-- %cov: -S instance
-- Not a generic body, so no possible instance tag.

--# sensors.ads
-- No expectation for this unit. Reference is needed here, still, to
-- get visibility on generic instantiations in this source.

--# check_g.adb
--  /test-out/   l+ ## 0
--  /in-range/   l+ ## 0
--  /out-range/  l+ ## 0

-- %cov: -S instance
--  =/test-out/   l! ## s-@(i:RF)
--  =/in-range/   l! ## s-@(i:RF)
--  =/out-range/  l! ## s-@(i:RF)
