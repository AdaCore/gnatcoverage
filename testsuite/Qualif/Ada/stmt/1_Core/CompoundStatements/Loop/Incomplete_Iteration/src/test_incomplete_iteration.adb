--  Test driver for LOOP statements. It executes only loops with EXIT
--  statements, and these EXITs transfer control out of the loop at the first
--  loop iteration, so part of the statements inside these loops should be
--  reported as non-covered.

with LOOP_Statements;         use LOOP_Statements;
with LOOP_Statements_Support; use LOOP_Statements_Support;
with Support;                 use Support;
procedure Test_Incomplete_Iteration is
   function My_Factorial is new Factorial (Natural);
   procedure My_Sum_First_Under_Limit is new Sum_First_Under_Limit (10);

   My_Sample : Sample (Identity (1) .. Identity (10)) :=
     (others => Level (Identity (13)));

   My_Big_Sample : Big_Sample (Identity (1) .. Identity (10)) :=
     (others => Identity (100));

   Res : Integer;
begin

   Assert (not All_Less_Then (Level (Identity (1)), My_Sample));

   Res := My_Factorial (0);
   Assert (Res = 1);

   My_Sum_First_Under_Limit (Res, My_Big_Sample);
   Assert (Res = 0);

end Test_Incomplete_Iteration;

--# loop_statements.adb
-- /preLoop1/              l+ 0
-- /loop1/                 l+ 0
-- /inloopbeforeexit1/     l+ 0
-- /inloopexit1/           l+ 0
-- /inloopafterexit1/      l- s-
-- /postLoop1/             l+ 0
-- /preLoop2/              l- s-
-- /loop2/                 l- s-
-- /inloopbeforeexit2/     l- s-
-- /inloopbeforeexitinif2/ l- s-
-- /inloopexit2/           l- s-
-- /preLoop3/              l+ 0
-- /loop3/                 ~l+ ~0
-- /inloopexit3/           l+ 0
-- /inloop3/               ~l- ~s-
-- /postLoop3/             l+ 0
-- /preLoop4/              l+ 0
-- /loop4/                 l+ 0
-- /inloopexit4/           l+ 0
-- /inloop4/               l- s-
