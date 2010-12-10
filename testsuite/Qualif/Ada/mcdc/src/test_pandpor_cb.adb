with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_CB is
begin
   Assert (F (True, False, False) = False);
   Assert (F (True, False, True) = True);
   Assert (F (True, True , False) = True);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! m!:"A"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
-- /decl/   l+ 0

