with Support, P1; use Support, P1;

procedure Test_346 is
   X : Boolean := False;
begin
   Combine (True, False, True, False); -- 3
   Assert (R = False);
   Combine (True, False, False, X); -- 4
   Assert (R = False);
   Combine (True, False, True, True); -- 6
   Assert (R = True);
end;

--# p1.adb
--  /comb/ l! ## c!:"A"
--  /or/   l! ## c!:"B"
