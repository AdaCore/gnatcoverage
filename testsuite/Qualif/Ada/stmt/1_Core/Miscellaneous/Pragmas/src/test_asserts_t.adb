with Support, Asserts; use Asserts, Support;

procedure Test_Asserts_T is
   R : Integer;
begin
   Sum (X => 2, Y => 3, UB => 10, R => R);
   Assert (R = 5);
   Dif (X => 5, Y => 1, LB => 1, R => R);
   Assert (R = 4);
end;

--# asserts.adb
-- /eval/ l+ 0
