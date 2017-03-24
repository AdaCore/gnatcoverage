with V6or9;

procedure Test_6m_9m is
begin
   V6or9.Check (V6inc  => False,
                V6mult => True,
                V9inc  => False, 
                V9mult => True);
end;

--# v6or9.adb
--  /stmt/  l+ ## 0
--  /6i/    l- ## s-
--  /6m/    l+ ## 0
--  /9i/    l- ## s-
--  /9m/    l+ ## 0

--# vops.adb
-- defaults, for instances conslidated:
--  /mult/   l+ ## 0
--  /inc/    l- ## s-

-- %cov: -S instance
--  =/mult/  l+ ## 0
--  =/inc/   l- ## s-@(i:V6), s-@(i:V9)

