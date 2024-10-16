with Contracts;

--  Test the absence of crash and the correctness of the coverage report when
--  both assertion and function/call coverage are activated simultaneously
--  when calling a function with a precondition.

procedure Test_Contracts is
begin
   Contracts.Foo (True);
end Test_Contracts;

--# contracts.ads
-- /pre/    l! ## ac!
--# contracts.adb
-- /t/      l+ ## 0
-- /assert/ l! ## ac!,c-
-- /null/   l+ ## 0
