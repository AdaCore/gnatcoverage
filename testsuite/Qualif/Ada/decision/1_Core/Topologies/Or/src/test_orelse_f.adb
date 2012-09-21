with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_F is
begin
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /orelse/   l! ## oT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
