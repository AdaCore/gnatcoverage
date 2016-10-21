with Support, Ops; use Support, Ops;

procedure Test_PV2_Inc is
   X : Integer := 21;
begin
   Process (X, Op_Inc, PV2);
   Assert (X = 22);
end;

--# ops.adb
--  /stmt/ l+ ## 0
--  /pv1_test_nop1/ l- ## s-
--  /pv1_nop1/ l- ## s-
--  /pv1_test_nop2/ l- ## s-
--  /pv1_nop2/ l- ## s-
--  /pv1_test_inc/  l- ## s-
--  /pv1_inc/ l- ## s-
--  /pv1_test_nop3/ l- ## s-
--  /pv1_nop3/ l- ## s-

--  /pv2_test/ l+ ## 0
--  /pv2_nop1/ l- ## s-
--  /pv2_nop2/ l- ## s-
--  /pv2_inc/  l+ ## 0
--  /pv2_nop3/ l- ## s-

--  /pv1_call/ l- ## s-
--  /pv2_call/ l+ ## 0

--  /do_inc/ l+ ## 0
--  /do_nop2/ l- ## s-
--  /do_nop3/ l- ## s-

--  %cargs: -O1
--  =/do_nop3/ ~l- ## ~s-

--# ops.ads
--  /do_nop1/ l- ## s-

--  %cargs: -O1
--  =/do_nop1/ ~l- ## ~s-

