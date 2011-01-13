with Support, CC4; use Support;

procedure Test_CC4_All is
begin
   for A in False .. True loop
      for B in False .. True loop
         for C in False .. True loop
            if A then
               Assert (CC4 (A, B, C) = B);
            else
               Assert (CC4 (A, B, C) = C);
            end if;
         end loop;
      end loop;
   end loop;
end;

--# cc4.adb
--  /eval/ u=>l!;l+ u=>c!:"not A"


