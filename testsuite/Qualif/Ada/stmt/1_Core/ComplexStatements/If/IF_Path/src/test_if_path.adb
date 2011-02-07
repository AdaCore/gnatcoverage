--  Test driver for IF statements. It executes all the IF statements from the
--  functional code in such a way that an IF path is chosen for each IF
--  statement.

with If_Statements;      use If_Statements;
with More_IF_Statements; use More_IF_Statements;
with Support;            use Support;
procedure Test_IF_Path is
   procedure My_Set_Max is new Set_Max (Integer);
   function My_Max_From_Two is new Max_From_Two (Integer);

   Res  : Integer;
begin

   Assert (not In_Range (1, 2, 3));

   My_Set_Max (Res, 2, 3);
   Assert (Res = 3);

   Set_Prime_Number (Res, 1);
   Assert (Res = 2);

   Assert (My_Max_From_Two (2, 1) = 2);

   Assert (Global_Var = 11);
end Test_IF_Path;

--# if_statements.adb
-- /XcmpMin/     l+ 0
-- /XoutMin/     l+ 0
-- /XcmpMax/     l- s-
-- /XoutMax/     l- s-
-- /Xin/         l- s-
-- /setmax/      l+ 0
-- /inifsetmax/  l+ 0

--# more_if_statements.adb
-- /prime/       l+ 0
-- /ifprime/     l+ 0
-- /1prime/      l+ 0
-- /comp2prime/  l- s-
-- /2prime/      l- s-
-- /comp3prime/  l- s-
-- /3prime/      l- s-
-- /comp4prime/  l- s-
-- /4prime/      l- s-
-- /comp5prime/  l- s-
-- /5prime/      l- s-
-- /comp6prime/  l- s-
-- /6prime/      l- s-
-- /comp7prime/  l- s-
-- /7prime/      l- s-
-- /comp8prime/  l- s-
-- /8prime/      l- s-
-- /comp9prime/  l- s-
-- /9prime/      l- s-
-- /comp10prime/ l- s-
-- /10prime/     l- s-
-- /max/         l+ 0
-- /ifmax/       l+ 0
--  /elsemax/    l- s-
-- /elab/        l+ 0
-- /gt0elab/     l+ 0
-- /notgt0elab/  l- s-
-- /eq0elabeq0/  l- s-
