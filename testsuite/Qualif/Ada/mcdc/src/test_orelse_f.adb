with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_F is
begin
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /evaluate/   l! dT-
--  /decisionTrue/  l- s-
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
