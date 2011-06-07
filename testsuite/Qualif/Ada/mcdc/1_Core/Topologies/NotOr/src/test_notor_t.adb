with Support, Notor; use Support, Notor;

procedure Test_Notor_T is
begin
   Assert (F (False, True) = True);
   Assert (F (True, True) = True);
   Assert (F (False, False) = True);
end;

--# notor.adb
--  /eval(Stmt|Other)/   l! dF-
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l- s-
--  /returnValue/   l+ 0
--  /decl/   l+ 0
