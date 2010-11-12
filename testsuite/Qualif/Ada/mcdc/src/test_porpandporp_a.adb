with Support, PorPandPorP;
use Support;

--  driver for (A or else B) and then (C or else D)
--
-- covering all conditions but A
-- B : {1, 2}
-- C : {2, 3}
-- D : {3, 4}
procedure Test_PorPandPorP_A is
begin
   Assert (PorPandPorP.F (False, False, True, True) = False); -- 1
   Assert (PorPandPorP.F (False, True, True, False) = True); -- 2
   Assert (PorPandPorP.F (False, True, False, False) = False); -- 3
   Assert (PorPandPorP.F (False, True, False, True) = True); -- 4
   Assert (PorPandPorP.F (True, False, False, False) = False); -- useless to cover "A"
end Test_PorPandPorP_A;

--# porpandporp.adb
--  /eval(Stmt|Other)/  l!  c!:"A"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
