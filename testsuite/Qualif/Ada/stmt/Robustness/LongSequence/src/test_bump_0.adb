with Support, Bump; use Support;

-- Verify that all the statements are reported uncovered when the
-- subprogram is not called.

procedure Test_Bump_0 is
begin
   Assert (True);
end;

--# bump.adb
--  /bump/  l- ## s-
