with Support, AndPorP; use Support, AndPorP;

--  covering B only
procedure Test_AndPorP_B is
begin
   Assert (F (True, True, False) = True);
   Assert (F (True, False, False) = False);
end;

--# andporp.adb
--  /eval(Stmt|Other)/      l! m!:"A",m!:"C"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
