--  Test driver for pragmas. It executes all the functional code, so nothing is
--  expected to be reported as uncovered.

with Support; use Support;
with Pragmas; use Pragmas;

procedure Test_Pragmas_Full is
begin
   Assert (Is_Safe (1));

   Assert (not In_Range (1, 2, 1));
   Assert (not In_Range (1, 2, 2));
   Assert (In_Range (3, 2, 4));
   Assert (not In_Range (5, 2, 4));
   Assert (not In_Range (1, 2, 4));
end Test_Pragmas_Full;

--# pragmas.adb
-- /pre_check_val/      l+ ## 0
-- /post_check_val/     l+ ## 0
-- /check_val/          l+ ## 0
-- /neverexecuted/      l- ## s-

-- /mainstream/         l+ ## 0
-- /nonemptyrange/      l+ ## 0
-- /morethenoneinrange/ l+ ## 0
-- /emptyrange/         l+ ## 0
-- /oneelement/         l+ ## 0
-- /XgtR/               l+ ## 0
-- /XltL/               l+ ## 0
-- /1debug/             l+ ## 0
-- /2debug/             l+ ## 0
-- /1assert/            l+ ## 0
-- /2assert/            l+ ## 0
-- /3assert/            l+ ## 0
-- /4assert/            l+ ## 0

-- /is_safe/            l+ ## 0
