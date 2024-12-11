--  Test the absence of crash and the correctness of the coverage report when
--  both assertion and function/call coverage are activated simultaneously
--  when never calling a function with a precondition.

procedure Test_Contracts_Not_Called is
begin
   null;
end Test_Contracts_Not_Called;

--# contracts.ads
-- /pre/    l- ## a-
--# contracts.adb
-- /foo/    l- ## f-
-- /t/      l- ## s-,f-
-- /assert/ l- ## s-,c-,c-
-- /null/   l- ## s-
