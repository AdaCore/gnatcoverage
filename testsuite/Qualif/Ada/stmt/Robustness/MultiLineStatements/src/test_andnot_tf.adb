with Support, Andnot; use Support;

--  Call and verify that nothing is reported uncovered.

procedure Test_Andnot_TF is
   E : Boolean;
begin
   Andnot (True, False, E);
   Assert (E = True);
end;

--# andnot.adb
--  /Statementmark/ l+ 0
--  /Linemark/      l+ 0c
