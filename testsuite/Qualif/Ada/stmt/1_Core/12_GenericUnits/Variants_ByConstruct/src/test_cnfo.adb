with Sensors; use Sensors;
procedure Test_CNFO is
begin
   --  F out of range

   Sensors.Test (0.0, F);
end Test_CNFO;

--# sensors.adb
-- /check-test-c/ l+ ## 0
-- /check-do-c/   l- ## s-
-- /check-do-f/   l+ ## 0

-- %cov: -S instance
-- Not a generic body, so no possible instance tag.

--# sensors.ads
-- No expectation for this unit. Reference is needed here, still, to
-- get visibility on generic instantiations in this source.

--# check_g.adb
--  /test-out/   l+ ## 0
--  /in-range/   l- ## s-
--  /out-range/  l+ ## 0

-- %cov: -S instance
--  =/test-out/ l! ## s-@(i:RC)
--  =/in-range/   l- ## s-@(i:RF), s-@(i:RC)
--  =/out-range/  l! ## s-@(i:RC)

