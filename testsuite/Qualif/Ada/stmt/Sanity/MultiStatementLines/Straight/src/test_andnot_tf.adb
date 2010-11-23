with Support, Andnot; use Support;

--  Call the provided service, covering all the statements. Verify that
--  partial coverage is reported on lines featuring multiple statements.

procedure Test_AndNot_TF is
   E : Boolean;
begin
   Andnot (True, False, E);
   Assert (E = True);
end;

--# andnot.adb
--  /doAndNot/  l! s!
