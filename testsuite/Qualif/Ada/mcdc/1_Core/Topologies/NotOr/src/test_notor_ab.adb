with Support, Notor; use Support, Notor;

procedure Test_Notor_AB is
begin
   Assert (F (True, False) = False);
   Assert (F (True, True) = True);
   Assert (F (False, False) = True);
end;

--# notor.adb
--  /eval(Stmt|Other)/   l+ ## 0
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
