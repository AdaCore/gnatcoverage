with BB_Null, Support; use BB_Null, Support;

-- Verify that when the noop subprogram is not called, only its nul+ 0
-- statement body is reported uncovered.

procedure Test_BB_Null_0 is
begin
   Assert (True);
end;

--# bb_null.adb
--  /nullInProc/ l- s-
--  /nullInElab/ l+ 0
