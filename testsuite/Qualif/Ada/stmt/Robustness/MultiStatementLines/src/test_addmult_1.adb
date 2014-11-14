with Support, Addmult; use Support;

--  Call the provided service, covering all the statements.

procedure Test_Addmult_1 is
   S, P : Integer;
begin
   Addmult (X => 5, Y => 4, S => S, P => P);
   Assert (S = 9);
   Assert (P = 20);
end;

--# addmult.adb
--  /compute/ l+ ## 0

-- %tags: (7.0.3|7.2.2)
-- =/compute/ l! ## s!
