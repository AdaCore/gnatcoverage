with Support, P1; use Support, P1;

procedure Test_46 is
   X : Boolean := True;
begin
   Combine (True, False, False, X); -- 4
   Assert (R = False);
   Combine (True, False, True, True); -- 6
   Assert (R = True);
end;


--# p1.adb
--  /comb/ l! ## c!:"A",c!:"D"
--  /or/   l! ## c!:"B"
