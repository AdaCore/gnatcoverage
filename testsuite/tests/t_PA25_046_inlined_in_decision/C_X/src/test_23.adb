with Support, P2; use Support, P2;

procedure Test_23 is
begin
   Combine (False, 15);
   Assert (R = True);
   Combine (True, 5);
   Assert (R = True);
   Combine (True, 15);
   Assert (R = True);
end;

--# p2.adb
--  /or/   l! ## eF-
--  /comp/ l+ ## 0
