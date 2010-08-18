with Support, Bump; use Support;

-- Verify that none of the statements are reported uncovered when the
-- subprogram is called.

procedure Test_Bump_All is
   X : Integer := 0;
begin
   Bump (X);
   Assert (X = 1000);
end;

--# bump.adb
--  /bump/  l+ 0
