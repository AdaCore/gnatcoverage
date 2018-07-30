--  Test driver for NULL statements. It executes some part of the functional
--  code that causes execution of some but not all NULL statements.

with Null_Statements; use Null_Statements;
with Support;         use Support;
procedure Test_Null_Statements_Part is
   Res : Integer;
   I   : Integer;
   J   : Integer;
begin
   Null_Proc_1 (1);
   Assert (True);

   Res := 3;
   I   := 1;
   J   := 1;

   Adjust (Res, I, J);
   Assert (Res = 3);

   Res := 2;
   Res := Adjust (Res, I, J);
   Assert (Res = 3);

   Set_Max (Res, 2, 1);
   Assert (Res = 2);

end Test_Null_Statements_Part;

--# null_statements.adb
-- /emptynull1/   l+ ## 0
-- /emptynull2/   l- ## s-
-- /caseproc/     l+ ## 0
-- /case1proc/    l- ## s-
-- /case2proc/    l- ## s-
-- /casenullproc/ l+ ## 0
-- /casefun/      l+ ## 0
-- /case1fun/     l- ## s-
-- /case2fun/     l+ ## 0
-- /casenullfun/  l- ## s-
-- /maxfirst/     l+ ## 0
-- /maxif/        l+ ## 0
-- /maxskip/      l- ## s-
-- /maxnull/      l+ ## 0
-- /minfirst/     l- ## s-
-- /minif/        l- ## s-
-- /minskip/      l- ## s-
-- /minnull/      l- ## s-
