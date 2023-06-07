with Support, P1; use Support, P1;

procedure Test_5 is
begin
   for X in False .. True loop
      Combine (True, True, X, True);
      Assert (R = True);
   end loop;
end;


--# p1.adb
--  /comb/ l! ## eF-
--  /or/   l! ## eF-
