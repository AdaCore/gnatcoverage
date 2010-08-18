--  Test driver for block statements. It executes some parts of the functional
--  code, but avoids enetering any straight-line statement sequence that
--  contains block statement. So some part of functional code is is expected to
--  be reported as covered, but no block statement or component thereof is
--  expected to be reported as covered.

with Block_Statements; use Block_Statements;
with Support;          use Support;
procedure Test_Block_Statements_No_Exec is
   function My_Factorial is new Factorial (Integer);
   procedure My_Swap is new Swap_G (Integer);

   Res1 : Integer := Identity (1);
   Res2 : Integer := Identity (1);

   Null_Sample  : Sample (Identity (1) .. Identity (0)) :=
     (others => Identity (13));
   Short_Sample : Sample := (1 => Identity (1));
begin
   My_Swap (Res1, Res2);
   Assert (Res1 = 1);

   Swap_Max_Min (Short_Sample);
   Assert (Short_Sample (1) = 1);

   Res1 := My_Factorial (-1);
   Assert (Res1 = 0);

   Res1 := Sum_Min_Max (Null_Sample);
   Assert (Res1 = 0);
end Test_Block_Statements_No_Exec;

--# block_statements.adb
-- /1if/              l+ 0
-- /1dclblock/        l- s-
-- /1stmtblock/       l- s-

-- /2if/              l+ 0
-- /2dclblock/        l- s-
-- /2stmtblock/       l- s-
-- /21ifstmtblock/    l- s-
-- /2in1ifstmtblock/  l- s-
-- /22ifstmtblock/    l- s-
-- /2in2ifstmtblock/  l- s-
-- /2stmtblock/       l- s-

-- /3stmt/            l+ 0
-- /3inif/            l+ 0
-- /3blockstmt/       l- s-
-- /3loopstmtblock/   l- s-
-- /3inloopstmtblock/ l- s-
-- /3handlerblock/    ~l- ~s-

-- /4stmt/            l+ 0
-- /4inif/            l+ 0
-- /4dclblock/        l- s-
-- /4blockstmt/       l- s-
-- /41ifstmtblock/    l- s-
-- /4in1ifstmtblock/  l- s-
-- /42ifstmtblock/    l- s-
-- /4in2ifstmtblock/  l- s-
