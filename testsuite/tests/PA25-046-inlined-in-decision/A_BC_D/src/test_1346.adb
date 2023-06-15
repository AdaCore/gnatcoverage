with Support, P1; use Support, P1;

procedure Test_1346 is
   X : Boolean := False;
begin
   Combine (False, X, X, X); -- 1
   Assert (R = False);
   Combine (True, False, True, False); -- 3
   Assert (R = False);
   Combine (True, False, False, X); -- 4
   Assert (R = False);
   Combine (True, False, True, True); -- 6
   Assert (R = True);
end;

--# p1.adb
--  /comb/ l+ ## 0
--  /or/   l! ## c!:"B"
