with Support, Notor; use Support, Notor;

procedure Test_Notor_A_SC is
begin
   Assert (F (False, True) = True);
   Assert (F (True, False) = False);
end;

--# notor.adb
--  /eval(Stmt|Other)/   l! ## c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   l+ ## 0
