with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_Amask is
begin
   Assert (Or_Else (True, True) = True);
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /eval(Stmt|Other)/   l! c!:"B"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
