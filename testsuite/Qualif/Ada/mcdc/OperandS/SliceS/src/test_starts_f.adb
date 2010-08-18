with Support, Starts; use Support;

procedure Test_Starts_F is
begin
   Assert (Starts ("hello", "blob") = False);
   Assert (Starts ("hello", "hellothere") = False);
end;

--# starts.adb
--  /startsLength/ l! dT-
--  /startsKey/    l! 0
