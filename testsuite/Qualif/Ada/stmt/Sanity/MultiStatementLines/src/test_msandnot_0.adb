with Support, Ms_Lines; use Support, Ms_Lines;

--  Call none of two provided services. Verify that all the
--  corresponding statement lines are reported uncovered.

procedure Test_MsAndNot_0 is
begin
   Assert (True);
end;

--# ms_lines.adb
--  /doAndNot/  l- s-
--  /setCarry/  l- s-
--  /doHalfAdd/ l- s-
