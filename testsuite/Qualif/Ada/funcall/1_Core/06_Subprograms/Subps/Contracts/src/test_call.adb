with Contracts;

--  Test the absence of crash and the correctness of the coverage report
--  when function/call coverage is activated for a subprogram with a
--  precondtion.

procedure Test_Call is
begin
   Contracts.Foo (True);
end Test_Call;

--# contracts.ads
-- /pre/    l. ## 0
--# contracts.adb

-- /t/      l+ ## 0
-- /assert/ l. ## 0
-- /null/   l+ ## 0
