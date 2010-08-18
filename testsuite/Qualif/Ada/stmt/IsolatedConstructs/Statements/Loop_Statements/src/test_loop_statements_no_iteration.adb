--  Test driver for LOOP statements. It executes only conditional loops from
--  the functional code, and the condition for each loop prevents loop
--  execution. So only loop statements but not statements from inside these
--  loops are expected to be reported as covered. This does not apply to the
--  loop statements from the package body (they are they are fully executed as
--  a part of package elaboration, so they are expected to be reported as
--  covered).

with LOOP_Statements;         use LOOP_Statements;
with More_LOOP_Statements;    use More_LOOP_Statements;
with LOOP_Statements_Support; use LOOP_Statements_Support;
with Support;                 use Support;
procedure Test_LOOP_Statements_No_Iteration is
   function My_Factorial is new Factorial (Natural);
   procedure My_Sum_First_Under_Limit is new Sum_First_Under_Limit (10);

   Null_Sample : Sample (Identity (1) .. Identity (0)) :=
     (others => Level (Identity (1)));

   Null_Big_Sample : Big_Sample (Identity (1) .. Identity (0)) :=
     (others => Identity (1));

   Null_String : String (Identity (1) .. Identity (0)) := (others => 'a');

   Res : Integer;
begin

   Assert (All_Less_Then (Level (Identity (13)), Null_Sample));

   Find_Last_Char (Res, Null_String, 'a');
   Assert (Res = 0);

   My_Sum_First_Under_Limit (Res, Null_Big_Sample);
   Assert (Res = 0);

   Assert (N_Of (Level (Identity (13)), Null_Sample) = 0);

   Change_Char (Null_String, 'a', 'b');
   Assert (Null_String'Length = 0);
end Test_LOOP_Statements_No_Iteration;

--# loop_statements.adb
-- /preLoop1/              l+ 0
-- /loop1/                 l+ 0
-- /inloopbeforeexit1/     l- s-
-- /inloopexit1/           l- s-
-- /inloopafterexit1/      l- s-
-- /postLoop1/             l+ 0
-- /preLoop2/              l+ 0
-- /loop2/                 l+ 0
-- /inloopbeforeexit2/     l- s-
-- /inloopbeforeexitinif2/ l- s-
-- /inloopexit2/           l- s-
-- /preLoop3/              ~l- ~s-
-- /loop3/                 ~l- ~s-
-- /inloopexit3/           ~l- ~s-
-- /inloop3/               ~l- ~s-
-- /postLoop3/             ~l- ~s-
-- /preLoop4/              l+ 0
-- /loop4/                 l+ 0
-- /inloopexit4/           ~l- ~s-
-- /inloop4/               ~l- ~s-

--# more_loop_statements.adb
-- /preLoop1/    l+ 0
-- /Loop1/       l+ 0
-- /inLoop1/     l- s-
-- /inIfinLoop1/ l- s-
-- /postLoop1/   l+ 0
-- /Loop2/       l+ 0
-- /inLoop2/     l- s-
-- /inIfinLoop2/ l- s-
-- /elab/        l+ 0
