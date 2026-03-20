with Support, P2; use Support, P2;

procedure Test_13 is
begin
   Combine (False, 5);
   Assert (R = False);
   Combine (True, 5);
   Assert (R = True);
   Combine (True, 15);
   Assert (R = True);
end;

--# p2.adb
--  /or/   l! ## c!:"F2"
--  /comp/ l+ ## 0
