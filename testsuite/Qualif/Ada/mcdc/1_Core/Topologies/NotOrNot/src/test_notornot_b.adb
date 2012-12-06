with Support, Notornot; use Support, Notornot;

procedure Test_Notornot_B is
begin
   Assert (F (True, True) = False);
   Assert (F (True, False) = True);
end;

--# notornot.adb
--  /eval(Stmt|Other)/   l! ## c!:"A"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
