with Support, Overloads; use Support, Overloads;

--  Arrange to call neither Flip (X) nor Flip (B). Verify that the calls and
--  body statements are reported uncovered for both.

procedure Test_Flip_0 is
begin
   Dispatch (Flipx => False, Flipb => False);
end;

--# overloads.adb
--  /flipx/  l- s-
--  /flipb/  l- s-
