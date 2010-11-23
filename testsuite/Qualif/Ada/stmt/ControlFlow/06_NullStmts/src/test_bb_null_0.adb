with BB_Null, Support; use BB_Null, Support;

--  When the noop subprogram is not called, its statement body only
--  is expected uncovered.

procedure Test_BB_Null_0 is
begin
   Assert (True);
end;

--# bb_null.adb
--  /nullInProc/ l- s-
--  /nullInElab/ l+ 0
