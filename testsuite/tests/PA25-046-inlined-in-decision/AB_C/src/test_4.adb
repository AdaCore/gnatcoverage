with Support, P1; use Support, P1;

procedure Test_4 is
begin
   for X in False .. True loop
      Combine (False, X, True);
      Assert (R = True);
   end loop;
end;


--# p1.adb
--  /and/ l! ## eT-
--  /or/  l! ## eF-
