with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_A is
begin
   Assert (F (False, True, False) = False);
   Assert (F (True, True, False) = True);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! ## c!:"B",c!:"C"
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   l+ ## 0

