with Support, P2; use Support, P2;

procedure Test_12 is
begin
   Combine (False, 5);
   Assert (R = False);
   Combine (False, 15);
   Assert (R = True);
end;

--# p2.adb
--  /or/   l! ## c!:"C"
--  /comp/ l+ ## 0
