with Support, Overloads; use Support, Overloads;

--  Arrange to call Flip (X) only.  Verify that the Flip (B) bits only are
--  reported uncovered (call & body).

procedure Test_Flip_X is
begin
   Dispatch (Flipb => False, Flipx => True);
end;

--# overloads.adb
--  /flipx/  l+ 0
--  /flipb/  l- s-
