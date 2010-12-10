with Support, CC4; use Support;

procedure Test_CC4_Masking is
   Any : constant Boolean := True;
begin
   Assert (CC4 (True, True, Any) = True);
   Assert (CC4 (False, Any, False) = False);
   Assert (CC4 (True, False, Any) = False);
   Assert (CC4 (False, Any, True) = True);
end;

--# cc4.adb
--  /eval/ ul!;l+ u!:"not A"


