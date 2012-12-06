with Support, Andidor; use Support, Andidor;

procedure Test_AndIdOr_F is
begin
   Assert (F (False, False, False) = False);
   Assert (F (False, False, False) = False);
   Assert (F (False, False, True)  = False);
   Assert (F (False, True , False) = False);
end;

--# andidor.adb
-- /eval(Stmt|Other)/  l! ## oT-:"A",e-:"B"
-- /decisionTrue/  l- ## s-
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   ~l+ ## 0
