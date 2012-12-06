with Support, Ornot; use Support, Ornot;

procedure Test_Ornot_T is
begin
   Assert (F (True, False) = True);
   Assert (F (False, False) = True);
   Assert (F (True, True) = True);
end;

--# ornot.adb
--  /eval(Stmt|Other)/   l! ## oF-
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l- ## s-
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
