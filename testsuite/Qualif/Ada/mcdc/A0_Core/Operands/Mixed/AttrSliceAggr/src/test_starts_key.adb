with Support, Starts; use Support;

procedure Test_Starts_Key is
begin
   Assert (Starts ("hello", 1, 'h') = True);
   Assert (Starts ("hello", 2, 'x') = False);
end;

--# starts.adb
--  /startsLength/ l! ## c!:"Len <="
--  /startsKey/    l! ## 0
