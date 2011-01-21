with Sort, Support; use Support;

procedure Test_Sort_GT is
begin
   Assert (Sort (X => 15, Min => 4, Max => 12) = 1);
end;

--# sort.adb
-- /testmin/ l+ 0
-- /ltmin/   l- s-
-- /testmax/ l+ 0
-- /gtmax/   l+ 0
-- /inrange/ l- s-
