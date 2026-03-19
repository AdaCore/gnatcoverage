with Support, P1; use Support, P1;

procedure Test_1 is
begin
   for Xbc in False .. True loop
      for Xd in False .. True loop
         Combine (False, Xbc, Xbc, Xd);
         Assert (R = False);
      end loop;
   end loop;
end;


--# p1.adb
--  /comb/ l! ## eT-
--  /or/   l- ## s-
