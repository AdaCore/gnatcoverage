with Support, P1; use Support, P1;

procedure Test_3 is
begin
   for X in False .. True loop
      Combine (True, True, X);
      Assert (R = True);
   end loop;
end;


--# p1.adb
--  /and/ l! ## eF-
--  /or/  l! ## eF-
