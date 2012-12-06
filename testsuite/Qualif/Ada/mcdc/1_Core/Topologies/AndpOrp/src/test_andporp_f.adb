with Support, AndPorP; use Support, AndPorP;

--  evaluating the decision to False only
procedure Test_AndPorP_F is
begin
   Assert (F (False, True, False) = False);
   Assert (F (True, False, False) = False);
end;

--# andporp.adb
--  /eval(Stmt|Other)/      l! ## oT-
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l+ ## 0
-- /returnValue/ l+ ## 0
-- /decl/ ~l+ ## 0
