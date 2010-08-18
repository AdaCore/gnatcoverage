with BB_Null, Support; use BB_Null, Support;

-- Verify that nothing is reported uncovered when the noop subprogram
-- is called.

procedure Test_BB_Null_All is
begin
   Do_Nothing;
   Assert (True);
end;

--# bb_null.adb
--  /nullIn(Proc|Elab)/ l+ 0
