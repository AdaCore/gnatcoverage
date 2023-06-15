with Support, P1; use Support, P1;

procedure Test_4 is
   X : Boolean := False;
begin
   Andthen (False, X, X);
   Assert (R = False);
end;

--# p1.adb
--  /comb/ l! ## eT-
