with Support, Overloads; use Support, Overloads;

procedure Test_Flip_B is
begin
   Dispatch (Flipb => True, Flipx => False);
end;

--# overloads.adb
--  /flipx/  l- s-
--  /flipb/  l+ 0
