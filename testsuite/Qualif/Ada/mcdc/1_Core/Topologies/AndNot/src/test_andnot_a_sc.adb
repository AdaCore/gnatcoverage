with Support, Andnot; use Support, Andnot;

procedure Test_Andnot_A_SC is
begin
   Assert (F (True, False) = True);
   Assert (F (False, True) = False);
end;

--# andnot.adb
--  /eval(Stmt|Other)/  l! c!:"B"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
