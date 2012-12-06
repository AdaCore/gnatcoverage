with Support, Andnot; use Support, Andnot;

procedure Test_Andnot_F is
begin
   Assert (F (False, True) = False);
   Assert (F (False, False) = False);
   Assert (F (True, True) = False);
end;

--# andnot.adb
--  /eval(Stmt|Other)/  l! ## oT-
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

