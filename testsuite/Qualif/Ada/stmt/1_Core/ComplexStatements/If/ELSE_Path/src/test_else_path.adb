--  Test driver for IF statements. Executes only those IF statements that
--  contain ELSe paths, does this in such a way that the ELSE path is chosen
--  for each statement being executed.

with If_Statements;      use If_Statements;
with More_IF_Statements; use More_IF_Statements;
with Support;            use Support;
procedure Test_ELSE_Path is
   procedure My_Set_Max is new Set_Max (Integer);
   function My_Max_From_Two is new Max_From_Two (Integer);

   Res : Integer;
begin

   Assert (In_Range (2, 1, 3));

   My_Set_Max (Res, 1, 0);
   Assert (Res = 1);

   Assert (My_Max_From_Two (1, 3) = 3);

   Assert (Global_Var = 11);
end Test_ELSE_Path;

--# if_statements.adb
-- /XcmpMin/     l+ 0
-- /XoutMin/     l- s-
-- /XcmpMax/     l+ 0
-- /XoutMax/     l- s-
-- /Xin/         l+ 0
-- /setmax/      l+ 0
-- /inifsetmax/  l- s-

--# more_if_statements.adb
-- /prime/       l- s-
-- /ifprime/     l- s-
-- /1prime/      l- s-
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
-- /ifmax/       l- s-
--  /elsemax/    l+ 0
-- /elab/        l+ 0
-- /gt0elab/     l+ 0
-- /notgt0elab/  l- s-
-- /eq0elabeq0/  l- s-
