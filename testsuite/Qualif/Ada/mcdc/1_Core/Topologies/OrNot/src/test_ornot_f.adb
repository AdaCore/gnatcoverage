with Support, Ornot; use Support, Ornot;

procedure Test_Ornot_F is
begin
   Assert (F (False, True) = False);
end;

--# ornot.adb
--  /eval(Stmt|Other)/   l! oT-
--  /decisionTrue/  l- s-
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
