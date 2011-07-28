with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_T is
begin
   Assert (F (True, True, True)  = True);
   Assert (F (True, False, True) = True);
   Assert (F (True, True, False) = True);
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! oF-:"A",eF-:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l- s-
-- /returnValue/   l+ 0
-- /decl/   l+ 0
