with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_AC is
begin
   Assert (F (False, True, False) = False); -- A C
   Assert (F (False, True, True)  = True);  --   C
   Assert (F (True , True, False) = True);  -- A
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! ## c!:"B"
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   l+ ## 0

