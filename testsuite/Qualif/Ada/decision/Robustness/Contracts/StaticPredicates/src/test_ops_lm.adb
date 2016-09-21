with Ops; use Ops;

procedure Test_Ops_LM is
begin
   Check_Late (Saturday);
   Check_Mid (Thursday);
end;

--# ops.ads

--# ops.adb
--  /test_early/ l! ## dT-
--  /early/ l- ## s-
--  /test_late/ l+ ## 0
--  /late/ l+ ## 0
--  /mid/ l+ ## 0
--  /check_early/ l- ## s-
--  /check_mid/ l+ ## 0
--  /check_late/ l+ ## 0
