--  Test driver for GOTO statements. It causes execution of the first GOTO
--  statement the functional code.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_1 is
   function My_Update is new Update_G (Integer);
begin
   Assert (My_Update (Identity (2)) = 4);
end Test_GOTO_Statements_1;

--# goto_statements.adb
-- /1if/         l+ 0
-- /1goto/       l+ 0
-- /2if/         l- s-
-- /2goto/       l- s-
-- /after2goto/  l- s-
-- /3goto/       l- s-
-- /after3goto/  l+ 0
-- /4goto/       l+ 0
-- /after4goto/  l- s-
-- /fin/         l+ 0
