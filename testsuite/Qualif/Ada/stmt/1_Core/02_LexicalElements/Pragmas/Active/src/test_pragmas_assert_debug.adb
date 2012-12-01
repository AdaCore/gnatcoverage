--  Test driver for pragmas. It calls a subprogram that has Precondition and
--  Postcondition pragmas associated with it, so these pragmas are expected to
--  be reported as covered, and Debug and Assert pragmas - as uncovered.

with Support; use Support;
with Pragmas; use Pragmas;

procedure Test_Pragmas_Assert_Debug is
begin
   Assert (not In_Range (1, 2, 1));
   Assert (not In_Range (1, 2, 2));
   Assert (In_Range (3, 2, 4));
   Assert (not In_Range (5, 2, 4));
   Assert (not In_Range (1, 2, 4));
end Test_Pragmas_Assert_Debug;

--# pragmas.adb
-- /pre_check_val/      l- ## s-
-- /post_check_val/     l- ## s-
-- /check_val/          l- ## s-
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

-- /safedecl/          ~l- ## ~s-
-- /is_safe/            l- ## s-
