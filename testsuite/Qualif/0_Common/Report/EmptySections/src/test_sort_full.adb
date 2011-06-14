with Sort, Support; use Support;

procedure Test_Sort_Full is
begin
   Assert (Sort (X => 3, Min => 4, Max => 12) = -1);
   Assert (Sort (X => 5, Min => 4, Max => 12) = 0);
   Assert (Sort (X => 15, Min => 4, Max => 12) = 1);
end;

--# sort.adb
-- /testmin/ l+ 0
-- /ltmin/   l+ 0
-- /testmax/ l+ 0
-- /gtmax/   l+ 0
-- /inrange/ l+ 0
