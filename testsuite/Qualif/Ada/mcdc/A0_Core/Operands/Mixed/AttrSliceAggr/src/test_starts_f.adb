with Support, Starts; use Support;

procedure Test_Starts_F is
begin
   Assert (Starts ("hello", 15, 'h') = False);
   Assert (Starts ("hello", 2, 'h') = False);
   Assert (Starts ("hello", 1, 'x') = False);
end;

--# starts.adb
--  /startsLength/ l! ## eT-
--  /startsKey/    l! ## 0
