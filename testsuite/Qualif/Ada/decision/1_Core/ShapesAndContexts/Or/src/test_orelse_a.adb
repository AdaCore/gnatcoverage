with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_A is
begin
   Assert (Or_Else (True, False) = True);
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /orelse/   l+ 0
--  /retTrue/  l+ 0
--  /retFalse/ l+ 0
--  /retVal/   l+ 0
