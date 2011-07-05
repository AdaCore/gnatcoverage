with Sort, Support; use Support;

procedure Test_Sort_IN is
begin
   Assert (Sort (X => 5, Min => 4, Max => 12) = 0);
end;

--# sort.adb
-- /testmin/ l+ 0
-- /ltmin/   l- s-
-- /testmax/ l+ 0
-- /gtmax/   l- s-
-- /inrange/ l+ 0
