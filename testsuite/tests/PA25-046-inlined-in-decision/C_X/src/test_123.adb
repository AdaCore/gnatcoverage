with Support, P2; use Support, P2;

procedure Test_123 is
begin
   Combine (False, 5);
   Assert (R = False);
   Combine (False, 15);
   Assert (R = True);
   Combine (True, 5);
   Assert (R = True);
   Combine (True, 15);
   Assert (R = True);
end;

--# p2.adb
--  /or/   l+ ## 0
--  /comp/ l+ ## 0

