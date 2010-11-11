--  Test driver for NULL statements. It executes all the functional code
--  making all the NULL statements executed.

with Null_Statements; use Null_Statements;
with Support;         use Support;
procedure Test_Null_Statements_Full is
   Res : Integer;
   I   : Integer;
   J   : Integer;
begin
   Null_Proc_1 (1);
   Assert (True);

   Null_Proc_2 (True);
   Assert (True);

   Res := 3;
   I   := 1;
   J   := 1;

   Adjust (Res, I, J);
   Assert (Res = 3);

   Res := 4;
   Res := Adjust (Res, 1, 1);
   Assert (Res = 4);

   Set_Max (Res, 2, 1);
   Assert (Res = 2);

   Set_Min (Res, 2, 1);
   Assert (Res = 1);

end Test_Null_Statements_Full;

--# null_statements.adb
-- /emptynull1/   l+ 0
-- /emptynull2/   l+ 0
-- /caseproc/     l+ 0
-- /case1proc/    l- s-
-- /case2proc/    l- s-
-- /casenullproc/ l+ 0
-- /casefun/      l+ 0
-- /case1fun/     l- s-
-- /case2fun/     l- s-
-- /casenullfun/  l+ 0
-- /max/          l+ 0
-- /maxif/        l+ 0
-- /amaxskip/     l- s-
-- /maxnull/      l+ 0
-- /min/          l+ 0
-- /aminif/       l- s-
-- /aminskip/     l+ 0
-- /minnull/      l+ 0
