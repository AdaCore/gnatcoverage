--  Test driver for GOTO statements. It executes all the functional code, so
--  everything is expected to be reported as covered.

with GOTO_Statements;      use GOTO_Statements;
with Support;              use Support;
procedure Test_GOTO_Statements_Full is
   function My_Update is new Update_G (Integer);
begin
   Assert (My_Update (Identity (-1)) = 1);
   Assert (My_Update (Identity (0)) = 1);
   Assert (My_Update (Identity (2)) = 4);
end Test_GOTO_Statements_Full;

--# goto_statements.adb
-- /1if/         l+ 0
-- /1goto/       l+ 0
-- /2if/         l+ 0
-- /2goto/       l+ 0
-- /after2goto/  l+ 0
-- /3goto/       l+ 0
-- /after3goto/  l+ 0
-- /4goto/       l+ 0
-- /after4goto/  l+ 0
-- /fin/         l+ 0
