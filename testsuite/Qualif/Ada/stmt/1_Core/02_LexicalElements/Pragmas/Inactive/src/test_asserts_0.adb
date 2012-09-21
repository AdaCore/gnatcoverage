with Support, Asserts; use Asserts, Support;

--  Don't call into the functional code. Expect s- only on active lines
--  with real statements, not on the pragma ones since deactivated by the
--  check policy.

procedure Test_Asserts_0 is
begin
   null;
end;

--# asserts.adb
-- /eval/   l- ## s-
-- /assert/ l. ## 0
