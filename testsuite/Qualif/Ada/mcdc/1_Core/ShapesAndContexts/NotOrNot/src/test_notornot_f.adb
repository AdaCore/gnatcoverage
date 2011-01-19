with Support, Notornot; use Support, Notornot;

procedure Test_Notornot_F is
begin
   Assert (F (True, True) = False);
end;

--# notornot.adb
--  /eval(Stmt|Other)/   l! dT-
--  /decisionTrue/  l- s-
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
