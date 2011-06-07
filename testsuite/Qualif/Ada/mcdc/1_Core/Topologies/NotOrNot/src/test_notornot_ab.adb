with Support, Notornot; use Support, Notornot;

procedure Test_Notornot_AB is
begin
   Assert (F (True, True) = False);
   Assert (F (True, False) = True);
   Assert (F (False, True) = True);
end;

--# notornot.adb
--  /eval(Stmt|Other)/   l+ 0
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
