with Support, Ops; use Support, Ops;

procedure Test_PV1_NOP3 is
   X : Integer := 21;
begin
   Process (X, Op_Nop3, PV1);
   Assert (X = 21);
end;

--# ops.adb
--  /stmt/ l+ ## 0
--  /pv1_test_nop1/ l+ ## 0
--  /pv1_nop1/ l- ## s-
--  /pv1_test_nop2/ l+ ## 0
--  /pv1_nop2/ l- ## s-
--  /pv1_test_inc/  l+ ## 0
--  /pv1_inc/ l- ## s-
--  /pv1_test_nop3/ l+ ## 0
--  /pv1_nop3/ l+ ## 0

--  /pv2_test/ l- ## s-
--  /pv2_nop1/ l- ## s-
--  /pv2_nop2/ l- ## s-
--  /pv2_inc/  l- ## s-
--  /pv2_nop3/ l- ## s-

--  /pv1_call/ l+ ## 0
--  /pv2_call/ l- ## s-

--  /do_inc/ l- ## s-
--  /do_nop2/ l- ## s-
--  /do_nop3/ l+ ## 0

--# ops.ads
--  /do_nop1/ l- ## s-
