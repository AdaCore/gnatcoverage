with Support, P1; use Support, P1;

procedure Test_3 is
begin
   Combine (True, False, True, False);
   Assert (R = False);
end;


--# p1.adb
--  /comb/ l! ## eT-
--  /or/   l! ## eF-
