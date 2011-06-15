with Support, BB_Pack; use Support, BB_Pack;

-- "with" the package featuring elaboration code only and verify that
-- nothing is reported uncovered.

procedure Test_BB_Pack_0 is
begin
   Assert (Sum = X + Y);
   Assert (Sum_Plus_Prod = (X + Y) + (X * Y));
end;

--# bb_pack.adb
-- /elab/ l+ 0
