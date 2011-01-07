with Support, Starts; use Support;

procedure Test_Starts_Full is
begin
   Assert (Starts ("hello", 1, 'h') = True);
   Assert (Starts ("hello", 15, 'h') = False);
   Assert (Starts ("hello", 1, 'x') = False);
end;

--# starts.adb
--  /startsLength/ l+ 0
--  /startsKey/    l+ 0
