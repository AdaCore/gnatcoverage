with Support, Overloads; use Support, Overloads;

procedure Test_Flip_Full is
begin
   Dispatch (Flipb => True, Flipx => True);
end;

--# overloads.adb
--  /flipx/  l+ 0
--  /flipb/  l+ 0
