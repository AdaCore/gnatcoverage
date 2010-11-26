with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_IC is
begin
   Assert (F (True, False, True) = True);
   Assert (F (True, False, False) = False);
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! m!:"A",m!:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
