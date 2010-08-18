with Support, Overloads; use Support, Overloads;

procedure Test_Flip_X is
begin
   Dispatch (Flipb => False, Flipx => True);
end;

--# overloads.adb
--  /flipx/  l+ 0
--  /flipb/  l- s-
