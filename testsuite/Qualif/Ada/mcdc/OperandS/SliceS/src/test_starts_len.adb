with Support, Starts; use Support;

procedure Test_Starts_Len is
begin
   Assert (Starts ("hello", "he") = True);
   Assert (Starts ("hello", "hellothere") = False);
end;

--# starts.adb
--  /startsLength/ l! 0
--  /startsKey/    l! c!:"S \("
