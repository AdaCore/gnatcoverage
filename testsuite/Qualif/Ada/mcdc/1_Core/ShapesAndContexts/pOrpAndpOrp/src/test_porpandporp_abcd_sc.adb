with Support, PorPandPorP;
use Support;

--  driver for (A or else B) and then (C or else D)
--
-- covering all conditions, using short-circuit masking
-- A : {1, 5}
-- B : {1, 2}
-- C : {2, 3}
-- D : {3, 4}
procedure Test_PorPandPorP_ABCD_SC is
begin
   Assert (PorPandPorP.F (False, False, True, True) = False); -- 1
   Assert (PorPandPorP.F (False, True, True, False) = True); -- 2
   Assert (PorPandPorP.F (False, True, False, False) = False); -- 3
   Assert (PorPandPorP.F (False, True, False, True) = True); -- 4
   Assert (PorPandPorP.F (True, False, False, True) = True); --  5
end Test_PorPandPorP_ABCD_SC;

--# porpandporp.adb
--  /eval(Stmt|Other)/  l+ 0
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/ l+ 0
--  /decl/ l+ 0
