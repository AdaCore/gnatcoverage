with Support, BB_Pack; use Support;

-- "with" the package featuring elaboration code only and verify that
-- nothing is reported uncovered.

procedure Test_BB_Pack_0 is
begin
   Assert (BB_Pack.X = 0);
end;

--# bb_pack.adb
-- /elab/ l+ 0
