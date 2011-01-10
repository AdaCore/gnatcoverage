with Support, Starts; use Support;

procedure Test_Starts_T is
begin
   Assert (Starts ("hello", 1, 'h') = True);
end;

--# starts.adb
--  /startsLength/ l! dF-
--  /startsKey/    l! 0
