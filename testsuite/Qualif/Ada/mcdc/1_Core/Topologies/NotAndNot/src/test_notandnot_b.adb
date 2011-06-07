with Support, Notandnot; use Support, Notandnot;

procedure Test_Notandnot_B is
begin
   Assert (F (False, False) = True);
   Assert (F (False, True) = False);
end;

--# notandnot.adb
--  /eval(Stmt|Other)/  l! c!:"A"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
