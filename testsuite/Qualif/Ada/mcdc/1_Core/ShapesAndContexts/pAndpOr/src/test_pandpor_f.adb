with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_F is
begin
   Assert (F (False, False, False) = False);
   Assert (F (False, True, False) = False);
   Assert (F (True, False, False) = False);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! dT-
-- /decisionTrue/  l- s-
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
-- /decl/   l+ 0

