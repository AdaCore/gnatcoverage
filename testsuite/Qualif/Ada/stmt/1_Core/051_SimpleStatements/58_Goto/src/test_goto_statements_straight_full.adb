--  Test driver for GOTO statements. It executes all the functional code, so
--  everything is expected to be reported as covered.

with GOTO_Statements_Straight; use GOTO_Statements_Straight;
with Support;                  use Support;
procedure Test_GOTO_Statements_Straight_Full is
begin
   Assert (Map (Identity (-1)) = 1);
   Assert (Map (Identity (0)) = 1);
   Assert (Map (Identity (2)) = 4);
end Test_GOTO_Statements_Straight_Full;

--# goto_statements_straight.adb
-- /1if/         l+ ## 0
-- /1goto/       l+ ## 0
-- /2if/         l+ ## 0
-- /2goto/       l+ ## 0
-- /after2goto/  l+ ## 0
-- /3goto/       l+ ## 0
-- /after3goto/  l+ ## 0
-- /4goto/       l+ ## 0
-- /after4goto/  l+ ## 0
-- /fin/         l+ ## 0
