with Contracts;

--  Test the absence of crash and the correctness of the coverage report when
--  function/call coverage is activated when never calling a function with a
--  precondition.
--  Calls in assertions should have no associated source coverage obligation.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# contracts.ads
-- /pre/    l. ## 0

--# contracts.adb
-- /foo/    l- ## f-
-- /t/      l- ## s-,f-
-- /assert/ l. ## 0
-- /null/   l- ## s-
