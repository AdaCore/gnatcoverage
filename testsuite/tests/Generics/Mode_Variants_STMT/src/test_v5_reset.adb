with V5; use V5;

procedure Test_V5_Reset is
   V : V5.Vector;
begin
   V5.Apply (Op=>Reset, V=>V);
end;

--# vectors.adb
--  /apply_op/    l+ ## 0
--  /apply_reset/ l+ ## 0
--  /iterate/     l+ ## 0
--  /do_reset/    l+ ## 0

--  /do_bump/     l- ## s-
--  /apply_bump/  l- ## s-

-- %cov: -S instance
--  =/do_bump/  l- ## s-@(i:V5)
--  =/apply_bump/  l- ## s-@(i:V5)

-- %cov: -S routine
--  =/apply_bump/  l- ## s-@(v5__apply)
--  =/do_bump/     l- ## s-@(v5__do_bump)

-- -S instance needs visibility on ALIs where
--  instances of interest are

--# v5.ads
