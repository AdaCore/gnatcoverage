with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_T is
begin
   Assert (Or_Else (False, True) = True);
   Assert (Or_Else (True, False) = True);
end;

--# orelse.adb
--  /orelse/   l+;mu=>l! mu => dF-
--  /retTrue/  l+ 0
--  /retFalse/ l- s-
--  /retVal/   l+ 0
