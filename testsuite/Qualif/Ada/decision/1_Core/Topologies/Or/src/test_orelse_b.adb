with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_B is
begin
   Assert (Or_Else (False, False) = False);
   Assert (Or_Else (False, True) = True);
end;

--# orelse.adb
--  /orelse/   l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
