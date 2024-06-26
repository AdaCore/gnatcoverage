--  Test driver for pragmas. It only 'with's the functional code but does not
--  execute anything from it, so everything is expected to be reported as
--  uncovered.

with Pragmas;
with Support; use Support;

procedure Test_Pragmas_No is
begin
   null;
end Test_Pragmas_No;

--# pragmas.adb

-- /pre_check_val/      l. ## 0
-- /post_check_val/     l. ## 0
-- /check_val/          l- ## s-
-- /neverexecuted/      l- ## s-

-- /rangedecl/         ~l- ## ~s-
-- /mainstream/         l- ## s-
-- /nonemptyrange/      l- ## s-
-- /morethenoneinrange/ l- ## s-
-- /emptyrange/         l- ## s-
-- /oneelement/         l- ## s-
-- /XgtR/               l- ## s-
-- /XltL/               l- ## s-
-- /safedecl/          ~l- ## ~s-
-- /is_safe/            l- ## s-

-- /1debug/             l- ## s-
-- /2debug/             l- ## s-

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
