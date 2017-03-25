with Sensors; use Sensors;
procedure Test_CNFN is
begin
   null;
end Test_CNFN;

--# sensors.adb
-- /check-test-c/ l- ## s-
-- /check-do-c/   l- ## s-
-- /check-do-f/   l- ## s-

-- %cov: -S instance
-- Not a generic body, so no possible instance tag.

--# sensors.ads
-- No expectation for this unit. Reference is needed here, still, to
-- get visibility on generic instantiations in this source.

--# check_g.adb
--  /test-out/   l- ## s-
--  /in-range/   l- ## s-
--  /out-range/  l- ## s-

-- %cov: -S instance
--  =/test-out/ l- ## s-@(i:RF), s-@(i:RC)
--  =/in-range/   l- ## s-@(i:RF), s-@(i:RC)
--  =/out-range/  l- ## s-@(i:RF), s-@(i:RC)

