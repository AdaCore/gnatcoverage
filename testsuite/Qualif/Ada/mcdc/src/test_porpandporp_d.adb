with Support, PorPandPorP;
use Support;

--  driver for (A or else B) and then (C or else D)
--
-- covering all conditions but D
-- A : {1, 2}
-- B : {1, 3}
-- C : {3, 4}
procedure Test_PorPandPorP_D is
begin
   Assert (PorPandPorP.F (False, False, True, True) = False); -- 1
   Assert (PorPandPorP.F (True, False, True, False) = True); -- 2
   Assert (PorPandPorP.F (False, True, True, False) = True); -- 3
   Assert (PorPandPorP.F (False, True, False, False) = False); -- 4
   Assert (PorPandPorP.F (False, True, True, True) = True); -- useless to cover D
end Test_PorPandPorP_D;

--# porpandporp.adb
--  /eval(Stmt|Other)/  l!  c!:"D"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/ l+ 0
