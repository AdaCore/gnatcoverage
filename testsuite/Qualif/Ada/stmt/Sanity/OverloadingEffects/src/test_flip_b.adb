with Support, Overloads; use Support, Overloads;

--  With two subprograms called Flip, arrange to call only the one operating
--  on Boolean. Verify that statements in the body of the other one (and only
--  them) are reported uncovered.

procedure Test_Flip_B is
begin
   Dispatch (Flipb => True, Flipx => False);
end;

--# overloads.adb
--  /flipx/  l- s-
--  /flipb/  l+ 0
