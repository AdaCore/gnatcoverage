--  Test driver for IF statements. Execute all the functional IF statements
--  arranging to take the [implicit] ELSE paths only.

with If_Statements;      use If_Statements;
with More_IF_Statements; use More_IF_Statements;
with Instances, Support; use Instances, Support;

procedure Test_ELSE_Path is
   Res : Integer;
begin

   Assert (In_Range (2, 1, 3));

   My_Set_Max (Res, 1, 0);
   Assert (Res = 1);

   Assert (My_Max_From_Two (1, 3) = 3);

   Assert (Global_Var = 11);

   Set_Prime_Number (Num => 50, Res => Res);
   Assert (Res = 0);

end Test_ELSE_Path;

--# if_statements.adb
-- /XcmpMin/     l+ ## 0
-- /XoutMin/     l- ## s-
-- /XcmpMax/     l+ ## 0
-- /XoutMax/     l- ## s-
-- /Xin/         l+ ## 0
-- /setmax/      l+ ## 0
-- /inifsetmax/  l- ## s-

--# more_if_statements.adb
-- /prime/       l+ ## 0
-- /ifprime/     l+ ## 0
-- /1prime/      l- ## s-
-- /comp2prime/  l+ ## 0
-- /2prime/      l- ## s-
-- /comp3prime/  l+ ## 0
-- /3prime/      l- ## s-
-- /comp4prime/  l+ ## 0
-- /4prime/      l- ## s-
-- /comp5prime/  l+ ## 0
-- /5prime/      l- ## s-
-- /comp6prime/  l+ ## 0
-- /6prime/      l- ## s-
-- /comp7prime/  l+ ## 0
-- /7prime/      l- ## s-
-- /comp8prime/  l+ ## 0
-- /8prime/      l- ## s-
-- /comp9prime/  l+ ## 0
-- /9prime/      l- ## s-
-- /comp10prime/ l+ ## 0
-- /10prime/     l- ## s-
-- /max/         l+ ## 0
-- /ifmax/       l- ## s-
-- /elsemax/     l+ ## 0
-- /elab/        l+ ## 0
-- /gt0elab/     l+ ## 0
-- /notgt0elab/  l- ## s-
-- /eq0elabeq0/  l- ## s-
