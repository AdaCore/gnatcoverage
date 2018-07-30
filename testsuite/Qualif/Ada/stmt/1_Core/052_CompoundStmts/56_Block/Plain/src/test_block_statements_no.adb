--  Test driver for block statements. It only "with"s the functional code,
--  but does not execute anything from it, so everything is expected to be
--  reported as uncovered.

with Block_Statements;    use Block_Statements;
with Instances, Support;  use Instances, Support;

procedure Test_Block_Statements_No is
begin
   Assert (True);
end Test_Block_Statements_No;

--# block_statements.adb
-- /1if/              l- ## s-
-- /1dclblock/        l- ## s-
-- /1stmtblock/       l- ## s-

-- /2if/              l- ## s-
-- /2dclblock/        l- ## s-
-- /2stmtblock/       l- ## s-
-- /21ifstmtblock/    l- ## s-
-- /2in1ifstmtblock/  l- ## s-
-- /22ifstmtblock/    l- ## s-
-- /2in2ifstmtblock/  l- ## s-

-- /3decl/           ~l- ## ~s-
-- /3stmt/            l- ## s-
-- /3inif/            l- ## s-
-- /3blockstmt/       l- ## s-
-- /3loopstmtblock/   l- ## s-
-- /3inloopstmtblock/ l- ## s-
-- /3handlerblock/    l- ## s-

-- /4decl/           ~l- ## ~s-
-- /4stmt/            l- ## s-
-- /4inif/            l- ## s-
-- /4dclblock/        l- ## s-
-- /4blockstmt/       l- ## s-
-- /41ifstmtblock/    l- ## s-
-- /4in1ifstmtblock/  l- ## s-
-- /42ifstmtblock/    l- ## s-
-- /4in2ifstmtblock/  l- ## s-
