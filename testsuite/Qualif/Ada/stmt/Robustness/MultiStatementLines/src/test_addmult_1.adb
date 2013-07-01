with Support, Addmult; use Support;

--  Call the provided service, covering all the statements. Verify that
--  partial coverage is reported on lines featuring multiple statements.

procedure Test_Addmult_1 is
   S, P : Integer;
begin
   Addmult (X => 5, Y => 4, S => S, P => P);
   Assert (S = 9);
   Assert (P = 20);
end;

--# addmult.adb
--  /compute/ l! ## s!
