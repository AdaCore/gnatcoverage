with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_ABC is
begin
   Assert (F (True , True , False) = True);
   Assert (F (False, True , False) = False);
   Assert (F (False, True , True)  = True);
   Assert (F (True , False, False) = False);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l+ ## 0
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l+ ## 0
-- /returnValue/   l+ ## 0
-- /decl/   l+ ## 0

