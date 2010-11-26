with Support, PorPandPorP;
use Support;

--  driver for (A or else B) and then (C or else D)
--
-- covering all conditions for Masking MC/DC; D not covered for Unique Cause.
-- A : {1, 4}
-- B : {1, 2}
-- C : {2, 3}
-- D : {3, 4}
procedure Test_PorPandPorP_ABCD_Mask is
begin
   Assert (PorPandPorP.F (False, False, True, True) = False); -- 1
   Assert (PorPandPorP.F (False, True, True, False) = True); -- 2
   Assert (PorPandPorP.F (False, True, False, False) = False); -- 3
   Assert (PorPandPorP.F (True, False, False, True) = True); --  4
end Test_PorPandPorP_ABCD_Mask;

--# porpandporp.adb
--  /eval(Stmt|Other)/  l!  u!:"D"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/ l+ 0
