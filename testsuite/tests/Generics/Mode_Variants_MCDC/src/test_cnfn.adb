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

-- %cov: -S routine
-- =/check-test-c/ l- ## s-@(sensors__test)
-- =/check-do-c/  l- ## s-@(sensors__test)
-- =/check-do-f/  l- ## s-@(sensors__test)

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

-- %cov: -S routine %cargs: !-gnatn
--  =/test-out/  l- ## s-@(sensors__rf__test), s-@(sensors__rc__test)
--  =/in-range/  l- ## s-@(sensors__rf__test), s-@(sensors__rc__test)
--  =/out-range/  l- ## s-@(sensors__rc__test), s-@(sensors__rf__test)
