with Support, Notornot; use Support, Notornot;

procedure Test_Notornot_T is
begin
   Assert (F (False, False) = True);
   Assert (F (True, False) = True);
   Assert (F (False, True) = True);
end;

--# notornot.adb
--  /eval(Stmt|Other)/   l! dF-
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l- s-
--  /returnValue/   l+ 0
--  /decl/   l+ 0
