with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_C_SC is
begin
   Assert (F (False, False, True) = True);
   Assert (F (False, True, False) = False);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! c!:"A",c!:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0

