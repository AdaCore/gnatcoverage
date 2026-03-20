with Support, PorPandPorP;
use Support;

--  driver for (A or else B) and then (C or else D)
--
-- covering all conditions but B and C
-- A : {1, 2}
-- D : {2, 3}
procedure Test_PorPandPorP_BC is
begin
   Assert (PorPandPorP.F (False, False, True, True) = False); -- 1
   Assert (PorPandPorP.F (True, False, False, True) = True); --  2
   Assert (PorPandPorP.F (True, False, False, False) = False); -- 3
end Test_PorPandPorP_BC;

--# porpandporp.adb
--  /eval(Stmt|Other)/  l! ##  c!:"B", c!:"C"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/          ~l+ ## 0
