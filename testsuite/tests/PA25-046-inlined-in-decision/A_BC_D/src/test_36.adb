with Support, P1; use Support, P1;

procedure Test_36 is
begin
   Combine (True, False, True, False); -- 3
   Assert (R = False);
   Combine (True, False, True, True); -- 6
   Assert (R = True);
end;

--# p1.adb
--  /comb/ l! ## c!:"A",c!:"Or"
--  /or/   l! ## eF-
