with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_IB is
begin
   Assert (F (True, True, False) = True);
   Assert (F (True, False, False) = False);
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! m!:"A",m!:"C"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
-- /decl/   l+ 0
