with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_B is
begin
   Assert (F (True, False, False) = False); -- Id B
   Assert (F (True, True , False) = True);  -- Id B
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! ## c!:"A",c!:"C"
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   ~l+ ## 0
