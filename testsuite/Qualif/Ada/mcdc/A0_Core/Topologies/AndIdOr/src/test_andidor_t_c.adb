with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_C is
begin
   Assert (F (True, False, False) = False); -- Id C
   Assert (F (True, False, True)  = True);  -- Id C
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! ## c!:"A",c!:"B"
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   ~l+ ## 0
