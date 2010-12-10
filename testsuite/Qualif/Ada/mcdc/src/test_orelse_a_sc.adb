with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_A_SC is
begin
   Assert (Or_Else (True, True) = True);
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /eval(Stmt|Other)/   l! m!:"B"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
