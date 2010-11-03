with Support, Overloads; use Support, Overloads;

--  With two subprograms called Flip, arrange to call only the one operating
--  on Integer. Verify that statements in the body of the other one (and only
--  them) are reported uncovered.

procedure Test_Flip_X is
begin
   Dispatch (Flipb => False, Flipx => True);
end;

--# overloads.adb
--  /flipx/  l+ 0
--  /flipb/  l- s-
