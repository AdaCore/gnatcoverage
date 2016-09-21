with Ops; use Ops;

procedure Test_Ops_EL is
begin
   Check_Early (Tuesday);
   Check_Late (Saturday);
end;

--# ops.ads

--# ops.adb
--  /test_early/ l+ ## 0
--  /early/ l+ ## 0
--  /test_late/ l! ## dF-
--  /late/ l+ ## 0
--  /mid/ l- ## s-
--  /check_early/ l+ ## 0
--  /check_mid/ l- ## s-
--  /check_late/ l+ ## 0
