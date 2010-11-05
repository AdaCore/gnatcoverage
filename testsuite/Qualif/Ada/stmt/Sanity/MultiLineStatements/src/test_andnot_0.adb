with Support, Andnot; use Support;

--  Don't call anything and verify that a "statement not covered"
--  violation is reported only for lines with a start of statement.

procedure Test_Andnot_0 is
begin
   Assert (True);
end;

--# andnot.adb
--  /Linemark/      l- 0
--  /Statementmark/ l- s-
