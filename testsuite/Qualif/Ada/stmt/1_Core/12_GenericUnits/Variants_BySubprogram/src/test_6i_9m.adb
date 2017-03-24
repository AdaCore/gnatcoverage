with V6or9;

procedure Test_6i_9m is
begin
   V6or9.Check (V6inc  => True,
                V6mult => False,
                V9inc  => False, 
                V9mult => True);
end;

--# v6or9.adb
--  /stmt/  l+ ## 0
--  /6i/    l+ ## 0
--  /6m/    l- ## s-
--  /9i/    l- ## s-
--  /9m/    l+ ## 0

--# vops.adb
-- defaults, for instances consolidated:
--  /mult/   l+ ## 0
--  /inc/    l+ ## 0

-- %cov: -S instance
--  =/mult/  l! ## s-@(i:V6)
--  =/inc/   l! ## s-@(i:V9)

