--  Test driver for IF statements. It calls all the subprograms from the
--  functional code (more than once for some of these subprograms) to ensure
--  that all the paths of all the IF statements are executed.

with If_Statements;      use If_Statements;
with More_IF_Statements; use More_IF_Statements;
with Support;            use Support;
procedure Test_Multiple_Paths is
   procedure My_Set_Max is new Set_Max (Integer);
   function My_Max_From_Two is new Max_From_Two (Integer);

   Res  : Integer;
begin

   --  Covering all paths in IF_Statements.In_Range
   Assert (not In_Range (1, 2, 3));
   Assert (not In_Range (4, 2, 3));
   Assert (In_Range (2, 1, 3));

   --  Covering all paths in IF_Statements.Set_Max
   My_Set_Max (Res, 2, 1);
   Assert (Res = 2);
   My_Set_Max (Res, 1, 2);
   Assert (Res = 2);

   --  Covering all paths in More_IF_Statements.Set_Prime_Number
   Set_Prime_Number (Res, 1);
   Assert (Res = 2);
   Set_Prime_Number (Res, 2);
   Assert (Res = 3);
   Set_Prime_Number (Res, 3);
   Assert (Res = 5);
   Set_Prime_Number (Res, 4);
   Assert (Res = 7);
   Set_Prime_Number (Res, 5);
   Assert (Res = 11);
   Set_Prime_Number (Res, 6);
   Assert (Res = 13);
   Set_Prime_Number (Res, 7);
   Assert (Res = 17);
   Set_Prime_Number (Res, 8);
   Assert (Res = 19);
   Set_Prime_Number (Res, 9);
   Assert (Res = 23);
   Set_Prime_Number (Res, 10);
   Assert (Res = 29);
   Set_Prime_Number (Res, 11);
   Assert (Res = 0);

   --  Covering all paths in More_IF_Statements.Max_From_Two
   Assert (My_Max_From_Two (2, 1) = 2);
   Assert (My_Max_From_Two (1, 2) = 2);

   Assert (Global_Var = 11);
end Test_Multiple_Paths;

--# if_statements.adb
-- /XcmpMin/    l+ ## 0
-- /XoutMin/    l+ ## 0
-- /XcmpMax/    l+ ## 0
-- /XoutMax/    l+ ## 0
-- /Xin/        l+ ## 0
-- /setmax/     l+ ## 0
-- /inifsetmax/ l+ ## 0

--# more_if_statements.adb
-- /prime/       l+ ## 0
-- /ifprime/     l+ ## 0
-- /1prime/      l+ ## 0
-- /comp2prime/  l+ ## 0
-- /2prime/      l+ ## 0
-- /comp3prime/  l+ ## 0
-- /3prime/      l+ ## 0
-- /comp4prime/  l+ ## 0
-- /4prime/      l+ ## 0
-- /comp5prime/  l+ ## 0
-- /5prime/      l+ ## 0
-- /comp6prime/  l+ ## 0
-- /6prime/      l+ ## 0
-- /comp7prime/  l+ ## 0
-- /7prime/      l+ ## 0
-- /comp8prime/  l+ ## 0
-- /8prime/      l+ ## 0
-- /comp9prime/  l+ ## 0
-- /9prime/      l+ ## 0
-- /comp10prime/ l+ ## 0
-- /10prime/     l+ ## 0
-- /max/         l+ ## 0
-- /ifmax/       l+ ## 0
--  /elsemax/    l+ ## 0
-- /elab/        l+ ## 0
-- /gt0elab/     l+ ## 0
-- /notgt0elab/  l- ## s-
-- /eq0elabeq0/  l- ## s-
