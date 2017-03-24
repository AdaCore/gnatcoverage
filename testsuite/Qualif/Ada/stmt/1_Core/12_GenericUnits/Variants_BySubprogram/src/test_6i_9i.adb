with V6or9;

procedure Test_6i_9i is
begin
   V6or9.Check (V6inc  => True,
                V6mult => False,
                V9inc  => True,
                V9mult => False);
end;

--# v6or9.adb
--  /stmt/  l+ ## 0
--  /6i/    l+ ## 0
--  /6m/    l- ## s-
--  /9i/    l+ ## 0
--  /9m/    l- ## s-

--# vops.adb
-- defaults, for instances conslidated:
--  /mult/   l- ## s-
--  /inc/    l+ ## 0

-- %cov: -S instance
--  =/mult/  l- ## s-@(i:V6), s-@(i:V9)
--  =/inc/   l+ ## 0

