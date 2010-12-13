with Support, CC6; use Support;

procedure Test_CC6_All is
begin
   for t in 9 .. 11 loop
      for A in False .. True loop
         for B in False .. True loop
            for C in False .. True loop
               if t < 10 then
                  Assert (CC6 (t, A, B, C) = A);
               elsif t = 10 then
                  Assert (CC6 (t, A, B, C) = B);
               else
                  Assert (CC6 (t, A, B, C) = C);
               end if;
            end loop;
         end loop;
      end loop;
   end loop;
end;

--# cc6.adb
--  /line1/ u=>l!;l+ 0
--  /line2/ u=>l!;l+ 0
--  /line3/ u=>l!;l+ u!:"High"
