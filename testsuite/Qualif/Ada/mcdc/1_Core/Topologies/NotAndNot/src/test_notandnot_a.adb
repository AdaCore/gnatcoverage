with Support, Notandnot; use Support, Notandnot;

procedure Test_Notandnot_A is
begin
   Assert (F (False, False) = True);
   Assert (F (True, False) = False);
end;

--# notandnot.adb
--  /eval(Stmt|Other)/  l! ## c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
