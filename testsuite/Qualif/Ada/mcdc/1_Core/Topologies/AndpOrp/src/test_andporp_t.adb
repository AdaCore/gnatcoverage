with Support, AndPorP; use Support, AndPorP;

--  evaluating the decision to True only
procedure Test_AndPorP_T is
begin
   Assert (F (True, True, False) = True);
   Assert (F (True, False, True) = True);
end;

--# andporp.adb andporp.ads
--  /eval(Stmt|Other)/      l! ## oF-
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l- ## s-
-- /returnValue/ l+ ## 0
-- /decl/ ~l+ ## 0
