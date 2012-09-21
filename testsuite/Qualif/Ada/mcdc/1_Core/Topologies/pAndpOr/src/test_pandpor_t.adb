with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_T is
begin
   Assert (F (False, False, True) = True);
   Assert (F (False, True, True) = True);
   Assert (F (True, False, True) = True);
   Assert (F (True, True, False) = True);
   Assert (F (True, True, True) = True);
end;

--# pandpor.adb
-- /eval(Stmt|Other)/      l! ## oF-
-- /decisionTrue/  l+ ## 0
-- /decisionFalse/ l- ## s-
-- /returnValue/   l+ ## 0
-- /decl/   l+ ## 0

