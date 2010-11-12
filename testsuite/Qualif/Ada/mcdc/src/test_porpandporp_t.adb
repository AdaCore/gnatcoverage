with Support, PorPandPorP;
use Support;

--  driver for (A or else B) and then (C or else D)
--
-- evaluating the condition to True only

procedure Test_PorPandPorP_T is
begin
   Assert (PorPandPorP.F (False, True, True, False) = True); -- 2
   Assert (PorPandPorP.F (False, True, False, True) = True); -- 4
end Test_PorPandPorP_T;

--# porpandporp.adb
--  /eval(Stmt|Other)/  l!  dF-
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l- s-
--  /returnValue/ l+ 0
