with Ops; use Ops;

procedure Test_Ops_Late is
begin
   Check_Late (Friday);
end;

--# ops.ads

--# ops.adb
--  /test_early/ l! ## dT-
--  /early/ l- ## s-
--  /test_late/ l! ## dF-
--  /late/ l+ ## 0
--  /mid/ l- ## s-
--  /check_early/ l- ## s-
--  /check_mid/ l- ## s-
--  /check_late/ l+ ## 0
