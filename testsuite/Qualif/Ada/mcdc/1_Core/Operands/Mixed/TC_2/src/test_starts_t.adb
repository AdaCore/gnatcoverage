with Support, Starts; use Support;

procedure Test_Starts_T is
begin
   Assert (Starts ("hello", "he") = True);
end;

--# starts.adb
--  /startsLength/ l! dF-
--  /startsKey/    l! 0
