with Support, Overloads; use Support, Overloads;

--  Arrange to call Flip (B) only.  Verify that the Flip (X) bits
--  only are reported uncovered (call & body).

procedure Test_Flip_B is
begin
   Dispatch (Flipb => True, Flipx => False);
end;

--# overloads.adb
--  /flipx/  l- ## s-
--  /flipb/  l+ ## 0
