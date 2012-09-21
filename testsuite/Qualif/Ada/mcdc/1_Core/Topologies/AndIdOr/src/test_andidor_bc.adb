with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_BC is
begin
   Assert (F (False, False, False) = False); -- B C
   Assert (F (False, False, True)  = False); --   C
   Assert (F (False, True , False) = False); -- B
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! ## oT-:"A",e-:"B"
-- /decisionTrue/  l- ## s-
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   l+ ## 0
