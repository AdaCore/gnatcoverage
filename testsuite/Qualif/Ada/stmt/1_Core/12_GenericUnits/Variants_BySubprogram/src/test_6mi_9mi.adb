with V6or9;

procedure Test_6mi_9mi is
begin
   V6or9.Check (V6inc  => True,
                V6mult => True,
                V9inc  => True,
                V9mult => True);
end;

--# v6or9.adb
--  /stmt/  l+ ## 0
--  /6i/    l+ ## 0
--  /6m/    l+ ## 0
--  /9i/    l+ ## 0
--  /9m/    l+ ## 0

--# vops.adb
-- defaults, for instances conslidated:
--  /mult/   l+ ## 0
--  /inc/    l+ ## 0

-- %cov: -S instance
--  =/mult/  l+ ## 0
--  =/inc/   l+ ## 0
