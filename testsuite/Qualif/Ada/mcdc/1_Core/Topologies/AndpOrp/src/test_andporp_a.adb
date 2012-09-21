with Support, AndPorP; use Support, AndPorP;

--  covering A only
procedure Test_AndPorP_A is
begin
   Assert (F (False, True, True) = False);
   Assert (F (True, True, False) = True);
end;

--# andporp.adb
--  /eval(Stmt|Other)/      l! ## c!:"B",c!:"C"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
