--  Test driver for block statements. It executes some parts of the functional
--  code, trying to eneter any straight-line statement sequence that contains
--  a block statement. So some part of functional code is is expected to
--  be reported as uncovered, but all the block statements and component
--  thereof are expected to be reported as covered.

with Block_Statements;    use Block_Statements;
with Instances, Support;  use Instances, Support;

procedure Test_Block_Statements_Blocks is

   Res1 : Integer := Identity (1);
   Res2 : Integer := Identity (2);

   My_Sample : Sample :=
     (1 => Identity (2), 2 => Identity (1), 3 => Identity (3));
begin
   My_Swap (Res1, Res2);
   Assert (Res1 = 2);

   Swap_Max_Min (My_Sample);
   Assert (My_Sample (1) = 2 and then
           My_Sample (2) = 3 and then
           My_Sample (3) = 1);

   Res1 := My_Factorial (3);
   Assert (Res1 = 6);

   Res1 := Sum_Min_Max (My_Sample);
   Assert (Res1 = 4);
end Test_Block_Statements_Blocks;

--# block_statements.adb
-- /1if/              l+ ## 0
-- /1dclblock/        l+ ## 0
-- /1stmtblock/       l+ ## 0

-- /2if/              l+ ## 0
-- /2dclblock/        l+ ## 0
-- /2stmtblock/       l+ ## 0
-- /21ifstmtblock/    l+ ## 0
-- /2in1ifstmtblock/  l+ ## 0
-- /22ifstmtblock/    l+ ## 0
-- /2in2ifstmtblock/  l+ ## 0

-- /3stmt/            l+ ## 0
-- /3inif/            l- ## s-
-- /3blockstmt/       l+ ## 0
-- /3loopstmtblock/   l+ ## 0
-- /3inloopstmtblock/ l+ ## 0
-- /3handlerblock/    l- ## s-

-- /4stmt/            l+ ## 0
-- /4inif/            l- ## s-
-- /4dclblock/        l+ ## 0
-- /4blockstmt/       l+ ## 0
-- /41ifstmtblock/    l+ ## 0
-- /4in1ifstmtblock/  l+ ## 0
-- /42ifstmtblock/    l+ ## 0
-- /4in2ifstmtblock/  l+ ## 0
