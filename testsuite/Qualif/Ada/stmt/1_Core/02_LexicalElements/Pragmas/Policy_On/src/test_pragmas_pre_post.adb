--  Test driver for pragmas. It calls a subprogram that has Debug and Assert
--  pragmas in its statement sequence, so these pragmas are expected to
--  be reported as covered, and Precondition and Postcondition pragmas - as
--  uncovered.

with Support; use Support;
with Pragmas; use Pragmas;

procedure Test_Pragmas_Pre_Post is
begin
   Assert (Is_Safe (1));
end Test_Pragmas_Pre_Post;

--# pragmas.adb
-- /pre_check_val/      l. ## 0
-- /post_check_val/     l. ## 0
-- /check_val/          l+ ## 0
-- /neverexecuted/      l- ## s-

-- /rangedecl/         ~l- ## ~s-
-- /mainstream/         l- ## s-
-- /nonemptyrange/      l- ## s-
-- /morethenoneinrange/ l- ## s-
-- /emptyrange/         l- ## s-
-- /oneelement/         l- ## s-
-- /XgtR/               l- ## s-
-- /XltL/               l- ## s-
-- /is_safe/            l+ ## 0

-- /1debug/             l- ## s-
-- /2debug/             l- ## s-

-- Assert pragmas are activated explicitly in this testcase,
-- but considered always inactive for source traces

--%opts: --trace-mode=bin
-- /1assert/            l- ## s-
-- /2assert/            l- ## s-
-- /3assert/            l- ## s-
-- /4assert/            l- ## s-

--%opts: --trace-mode=src
-- /1assert/            l. ## 0
-- /2assert/            l. ## 0
-- /3assert/            l. ## 0
-- /4assert/            l. ## 0
