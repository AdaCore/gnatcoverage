with Support, CC4; use Support;

--  Invoke the functional decision with a minimal combination of inputs that
--  lead to satisfaction of masking mcdc. Verify UC mcdc (only) not satisfied.

procedure Test_CC4_Masking is
   Any : constant Boolean := True;
begin
   Assert (CC4 (True, True, Any) = True);
   Assert (CC4 (False, Any, False) = False);
   Assert (CC4 (True, False, Any) = False);
   Assert (CC4 (False, Any, True) = True);
end;

--# cc4.adb

--  /eval/ u=>l!;l+ ## u=>c!:"not A"


