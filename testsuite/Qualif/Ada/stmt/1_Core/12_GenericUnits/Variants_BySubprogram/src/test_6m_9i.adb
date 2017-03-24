with V6or9;

procedure Test_6m_9i is
begin
   V6or9.Check (V6inc  => False,
                V6mult => True,
                V9inc  => True, 
                V9mult => False);
end;

--# v6or9.adb
--  /stmt/  l+ ## 0
--  /6i/    l- ## s-
--  /6m/    l+ ## 0
--  /9i/    l+ ## 0
--  /9m/    l- ## s-

--# vops.adb
-- defaults, for instances conslidated:
--  /mult/   l+ ## 0
--  /inc/    l+ ## 0

-- %cov: -S instance
--  =/mult/  l! ## s-@(i:V9)
--  =/inc/   l! ## s-@(i:V6)

