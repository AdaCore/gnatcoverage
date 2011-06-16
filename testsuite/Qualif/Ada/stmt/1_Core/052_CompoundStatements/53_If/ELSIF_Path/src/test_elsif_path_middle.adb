--  Test driver for IF statements. Executes only the IF statement that contains
--  several ELSIF paths, does this in such a way that some ELSIF path in the
--  middle is chosen.

with More_IF_Statements; use More_IF_Statements;
with Support;            use Support;
procedure Test_ELSIF_Path_Middle is

   Res  : Integer;
begin

   Set_Prime_Number (Res, 6);
   Assert (Res = 13);

   Assert (Global_Var = 11);
end Test_ELSIF_Path_Middle;

--# more_if_statements.adb
-- /prime/       l+ 0
-- /ifprime/     l+ 0
-- /1prime/      l- s-
-- /comp2prime/  l+ 0
-- /2prime/      l- s-
-- /comp3prime/  l+ 0
-- /3prime/      l- s-
-- /comp4prime/  l+ 0
-- /4prime/      l- s-
-- /comp5prime/  l+ 0
-- /5prime/      l- s-
-- /comp6prime/  l+ 0
-- /6prime/      l+ 0
-- /comp7prime/  l- s-
-- /7prime/      l- s-
-- /comp8prime/  l- s-
-- /8prime/      l- s-
-- /comp9prime/  l- s-
-- /9prime/      l- s-
-- /comp10prime/ l- s-
-- /10prime/     l- s-
-- /max/         ~l- ~s-
-- /ifmax/       ~l- ~s-
--  /elsemax/    ~l- ~s-
-- /elab/        l+ 0
-- /gt0elab/     l+ 0
-- /notgt0elab/  l- s-
-- /eq0elabeq0/  l- s-
