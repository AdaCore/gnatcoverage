with Support, P1; use Support, P1;

procedure Test_3 is
   X : Boolean := False;
begin
   Andthen (True, False, X);
   Assert (R = False);
end;

--# p1.adb
--  /comb/ l! ## eT-
