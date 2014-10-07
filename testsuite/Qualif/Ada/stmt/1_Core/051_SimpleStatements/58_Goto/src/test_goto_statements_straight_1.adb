--  Test driver for GOTO statements. It causes execution of the first GOTO
--  statement the functional code.

with GOTO_Statements_Straight; use GOTO_Statements_Straight;
with Support;                  use Support;
procedure Test_GOTO_Statements_Straight_1 is
begin
   Assert (Map (Identity (2)) = 4);
end Test_GOTO_Statements_Straight_1;

--# goto_statements_straight.adb
-- /1if/         l+ ## 0
-- /1goto/       l+ ## 0
-- /2if/         l- ## s-
-- /2goto/       l- ## s-
-- /after2goto/  l- ## s-
-- /3goto/       l- ## s-
-- /after3goto/  l+ ## 0
-- /4goto/       l+ ## 0
-- /after4goto/  l- ## s-
-- /fin/         l+ ## 0
