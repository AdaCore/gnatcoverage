with Support, Notor; use Support, Notor;

procedure Test_Notor_B is
begin
   Assert (F (True, False) = False);
   Assert (F (True, True) = True);
end;

--# notor.adb
--  /eval(Stmt|Other)/   l! ## c!:"A"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
