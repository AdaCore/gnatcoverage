with Support, AndPorP; use Support, AndPorP;

--  covering C only
procedure Test_AndPorP_C is
begin
   Assert (F (True, False, True) = True);
   Assert (F (True, False, False) = False);
end;

--# andporp.adb andporp.ads
--  /eval(Stmt|Other)/      l! ## c!:"A",c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
