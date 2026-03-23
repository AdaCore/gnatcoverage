with Support, Starts; use Support;

procedure Test_Starts_Full is
begin
   Assert (Starts ("hello", "he") = True);
   Assert (Starts ("hello", "hellothere") = False);
   Assert (Starts ("hello", "blo") = False);
end;

--# starts.adb
--  /startsLength/ l+ ## 0
--  /startsKey/    l+ ## 0
