with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_A is
begin
   Assert (Or_Else (True, False) = True);
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /evaluate/   l! c!:"B"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
