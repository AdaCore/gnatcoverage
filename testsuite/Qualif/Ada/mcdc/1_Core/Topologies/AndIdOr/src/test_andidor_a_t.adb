with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_A_T is
begin
   Assert (F (True , True, True) = True);
   Assert (F (False, True, True) = False);
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! ## c!:"Id",eF-:"B"
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   l+ ## 0
