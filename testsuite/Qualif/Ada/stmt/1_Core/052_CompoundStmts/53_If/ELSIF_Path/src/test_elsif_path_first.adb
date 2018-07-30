--  Test driver for IF statements. Executes only those IF statements that
--  contain ELSIF paths, does this in such a way that the first ELSIF path is
--  chosen.

with If_Statements;      use If_Statements;
with More_IF_Statements; use More_IF_Statements;
with Support;            use Support;
procedure Test_ELSIF_Path_First is

   Res  : Integer;
begin

   Assert (not In_Range (4, 2, 3));

   Set_Prime_Number (Res, 2);
   Assert (Res = 3);

   Assert (Global_Var = 11);
end Test_ELSIF_Path_First;

--# if_statements.adb
-- /XcmpMin/     l+ ## 0
-- /XoutMin/     l- ## s-
-- /XcmpMax/     l+ ## 0
-- /XoutMax/     l+ ## 0
-- /Xin/         l- ## s-
-- /setmax/      ~l- ## ~s-
-- /inifsetmax/  ~l- ## ~s-

--# more_if_statements.adb
-- /prime/       l+ ## 0
-- /ifprime/     l+ ## 0
-- /1prime/      l- ## s-
-- /comp2prime/  l+ ## 0
-- /2prime/      l+ ## 0
-- /comp3prime/  l- ## s-
-- /3prime/      l- ## s-
-- /comp4prime/  l- ## s-
-- /4prime/      l- ## s-
-- /comp5prime/  l- ## s-
-- /5prime/      l- ## s-
-- /comp6prime/  l- ## s-
-- /6prime/      l- ## s-
-- /comp7prime/  l- ## s-
-- /7prime/      l- ## s-
-- /comp8prime/  l- ## s-
-- /8prime/      l- ## s-
-- /comp9prime/  l- ## s-
-- /9prime/      l- ## s-
-- /comp10prime/ l- ## s-
-- /10prime/     l- ## s-
-- /max/         ~l- ## ~s-
-- /ifmax/       ~l- ## ~s-
--  /elsemax/    ~l- ## ~s-
-- /elab/        l+ ## 0
-- /gt0elab/     l+ ## 0
-- /notgt0elab/  l- ## s-
-- /eq0elabeq0/  l- ## s-
