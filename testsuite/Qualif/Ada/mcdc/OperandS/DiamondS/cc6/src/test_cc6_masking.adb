with Support, CC6; use Support;

procedure Test_CC6_Masking is
   Any : constant Boolean := True;
begin
   Assert (CC6 (9, True, Any, Any) = True);
   Assert (CC6 (9, False, Any, Any) = False);
   Assert (CC6 (10, Any, True, Any) = True);
   Assert (CC6 (10, Any, False, Any) = False);
   Assert (CC6 (11, Any, Any, True) = True);
   Assert (CC6 (11, Any, Any, False) = False);
end;

--# cc6.adb
--  /line1/ l! 0
--  /line2/ l! 0
--  /line3/ l! u!:"High"
