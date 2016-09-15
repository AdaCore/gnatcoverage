with Ops; use Ops;

procedure Test_Ops_Early is
begin
   Check_Early (Monday);
end;

--# ops.ads

--# ops.adb
--  /test_early/ l! ## dF-
--  /early/ l+ ## 0
--  /test_late/ l- ## s-
--  /late/ l- ## s-
--  /mid/ l- ## s-
--  /check_early/ l+ ## 0
--  /check_mid/ l- ## s-
--  /check_late/ l- ## s-
