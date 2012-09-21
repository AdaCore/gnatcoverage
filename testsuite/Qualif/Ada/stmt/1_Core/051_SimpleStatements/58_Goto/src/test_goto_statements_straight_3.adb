--  Test driver for GOTO statements. It does not cause execution of the first
--  and of the second GOTO statement the functional code, but it causes
--  execution of the third GOTO statement

with GOTO_Statements_Straight; use GOTO_Statements_Straight;
with Support;                  use Support;
procedure Test_GOTO_Statements_Straight_3 is
   function My_Update is new Update_G (Integer);
begin
   Assert (My_Update (Identity (-1)) = 1);
end Test_GOTO_Statements_Straight_3;

--# goto_statements_straight.adb
-- /1if/         l+ ## 0
-- /1goto/       l- ## s-
-- /2if/         l+ ## 0
-- /2goto/       l- ## s-
-- /after2goto/  l+ ## 0
-- /3goto/       l+ ## 0
-- /after3goto/  l- ## s-
-- /4goto/       l- ## s-
-- /after4goto/  l- ## s-
-- /fin/         l+ ## 0
