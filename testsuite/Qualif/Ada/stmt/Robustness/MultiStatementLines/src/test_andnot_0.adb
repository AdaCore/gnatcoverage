with Support, Andnot; use Support;

--  Call nothing. Verify that all the statements are reported uncovered.

procedure Test_AndNot_0 is
begin
   Assert (True);
end;

--# andnot.adb
--  /doAndNot/  l- s-:"Not_B .= not B",s-:"E .= A "
