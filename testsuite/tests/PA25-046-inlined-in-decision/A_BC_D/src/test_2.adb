with Support, P1; use Support, P1;

procedure Test_2 is
begin
   for X in False .. True loop
      Combine (True, True, X, False);
      Assert (R = False);
   end loop;
end;


--# p1.adb
--  /comb/ l! ## eT-
--  /or/   l! ## eF-
