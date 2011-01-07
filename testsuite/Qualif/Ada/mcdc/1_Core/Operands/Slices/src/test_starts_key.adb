with Support, Starts; use Support;

procedure Test_Starts_Key is
begin
   Assert (Starts ("hello", "he") = True);
   Assert (Starts ("hello", "blo") = False);
end;

--# starts.adb
--  /startsLength/ l! c!:"Key.Length"
--  /startsKey/    l! 0
