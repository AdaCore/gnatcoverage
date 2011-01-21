with Sort, Support; use Support;

procedure Test_Sort_LT is
begin
   Assert (Sort (X => 3, Min => 4, Max => 12) = -1);
end;

--# sort.adb
-- /testmin/ l+ 0
-- /ltmin/   l+ 0
-- /testmax/ l- s-
-- /gtmax/   l- s-
-- /inrange/ l- s-
