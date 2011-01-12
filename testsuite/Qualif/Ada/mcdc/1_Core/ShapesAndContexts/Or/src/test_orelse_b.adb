with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_B is
begin
   Assert (Or_Else (False, False) = False);
   Assert (Or_Else (False, True) = True);
end;

--# orelse.adb
--  /eval(Stmt|Other)/   l! c!:"A"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
