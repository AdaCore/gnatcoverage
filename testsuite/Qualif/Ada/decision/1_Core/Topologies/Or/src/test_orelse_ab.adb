with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_AB is
begin
   Assert (Or_Else (False, False) = False);
   Assert (Or_Else (False, True) = True);
   Assert (Or_Else (True, False) = True);
end;

--# orelse.adb
--  /orelse/   l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
