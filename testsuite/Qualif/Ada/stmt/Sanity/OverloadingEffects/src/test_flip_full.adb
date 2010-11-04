with Support, Overloads; use Support, Overloads;

--  Arrange to call both Flip (X) and Flip (B). Verify that nothing is
--  reported uncovered.

procedure Test_Flip_Full is
begin
   Dispatch (Flipb => True, Flipx => True);
end;

--# overloads.adb
--  /flipx/  l+ 0
--  /flipb/  l+ 0
