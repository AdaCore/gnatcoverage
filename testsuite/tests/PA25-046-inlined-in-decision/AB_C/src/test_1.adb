with Support, P1; use Support, P1;

procedure Test_1 is
begin
   for X in False .. True loop
      Combine (False, X, False);
      Assert (R = False);
   end loop;
end;


--# p1.adb
--  /and/ l! ## eT-
--  /or/  l! ## eT-
