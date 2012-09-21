with Support, Andnot; use Support, Andnot;

procedure Test_Andnot_B is
begin
   Assert (F (True, False) = True);
   Assert (F (True, True) = False);
end;

--# andnot.adb
--  /eval(Stmt|Other)/  l! ## c!:"A"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   l+ ## 0
