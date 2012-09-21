with Support, Notornot; use Support, Notornot;

procedure Test_Notornot_A is
begin
   Assert (F (False, True) = True);
   Assert (F (True, True) = False);
end;

--# notornot.adb
--  /eval(Stmt|Other)/   l! ## c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   l+ ## 0
