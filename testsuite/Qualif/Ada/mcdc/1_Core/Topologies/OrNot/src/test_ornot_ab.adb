with Support, Ornot; use Support, Ornot;

procedure Test_Ornot_AB is
begin
   Assert (F (False, True) = False);
   Assert (F (False, False) = True);
   Assert (F (True, True) = True);
end;

--# ornot.adb
--  /eval(Stmt|Other)/   l+ ## 0
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
