with Support, Notandnot; use Support, Notandnot;

procedure Test_Notandnot_F is
begin
   Assert (F (True, True) = False);
   Assert (F (True, False) = False);
   Assert (F (False, True) = False);
end;

--# notandnot.adb
--  /eval(Stmt|Other)/  l! ## oT-
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   l+ ## 0

