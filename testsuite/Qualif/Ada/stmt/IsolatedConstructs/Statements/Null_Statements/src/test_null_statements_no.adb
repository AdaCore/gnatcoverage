--  Test driver for NULL statements. It only "with"s the functional code, but
--  executes only a small part of it that does not cause execution of any
--  NULL statement.

with Null_Statements; use Null_Statements;
with Support;         use Support;
procedure Test_Null_Statements_No is
   Res : Integer;
   I   : Integer;
   J   : Integer;
begin
   Res := 1;
   I   := 1;
   J   := 1;

   Adjust (Res, I, J);
   Assert (Res = 1);

   Res := Adjust (Res, I, J);
   Assert (Res = 1);
end Test_Null_Statements_No;

--# null_statements.adb
-- /emptynull1/   l- s-
-- /emptynull2/   l- s-
-- /case1proc/    l+ 0
-- /case2proc/    l- s-
-- /casenullproc/ l- s-
-- /casefun/      l+ 0
-- /case1fun/     l+ 0
-- /case2fun/     l- s-
-- /casenullfun/  l- s-
-- /max/          l- s-
-- /maxif/        l- s-
-- /amaxskip/     l- s-
-- /maxnull/      l- s-
-- /min/          l- s-
-- /aminif/       l- s-
-- /aminskip/     l- s-
-- /minnull/      l- s-
