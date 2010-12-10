with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_AB is
begin
   Assert (F (False, True , False) = False);  -- A
   Assert (F (True , False, False) = False);  --    B
   Assert (F (True , True , False) = True);   -- A  B
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! m!:"C"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
-- /decl/   l+ 0

