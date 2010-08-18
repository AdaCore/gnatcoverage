with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_AB is
begin
   Assert (Or_Else (False, False) = False);
   Assert (Or_Else (False, True) = True);
   Assert (Or_Else (True, False) = True);
end;

--# orelse.adb
--  /evaluate/   l+ 0
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
