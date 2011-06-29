--  Test driver for LOOP statements. Exercise the loop statements making sure
--  that they all iterate at least once over the entire nested sequence

with LOOP_Statements;         use LOOP_Statements;
with More_LOOP_Statements;    use More_LOOP_Statements;
with LOOP_Statements_Support; use LOOP_Statements_Support;
with Instances, Support;      use Instances, Support;

procedure Test_Full_Iteration is

   My_Sample : Sample :=
     (Level (Identity (1)),
      Level (Identity (2)),
      Level (Identity (3)));

   My_Big_Sample : Big_Sample :=
     (Identity (1),
      Identity (2),
      Identity (100));

   My_String : String (Identity (1) .. 4):= "abcd";

   Res : Integer;
begin

   Assert (All_Less_Then (Level (Identity (13)), My_Sample));

   Find_Last_Char (Res, My_String, 'a');
   Assert (Res = 1);

   Res := My_Factorial (3);
   Assert (Res = 6);

   My_Sum_First_Under_Limit (Res, My_Big_Sample);
   Assert (Res = 3);

   Assert (N_Of (Level (Identity (3)), My_Sample) = 1);

   Change_Char (My_String, 'a', 'b');
   Assert (My_String = "bbcd");
end Test_Full_Iteration;

--# loop_statements.adb
-- /preLoop1/              l+ 0
-- /loop1/                 l+ 0
-- /inloopbeforeexit1/     l+ 0
-- /inloopexit1/           l+ 0
-- /inloopafterexit1/      l+ 0
-- /postLoop1/             l+ 0
-- /preLoop2/              l+ 0
-- /loop2/                 l+ 0
-- /inloopbeforeexit2/     l+ 0
-- /inloopbeforeexitinif2/ l+ 0
-- /inloopexit2/           l+ 0
-- /preLoop3/              l+ 0
-- /inloopexit3/           l+ 0
-- /inloop3/               l+ 0
-- /postLoop3/             l+ 0
-- /preLoop4/              l+ 0
-- /loop4/                 l+ 0
-- /inloopexit4/           l+ 0
-- /inloop4/               l+ 0

--# more_loop_statements.adb
-- /preLoop1/              l+ 0
-- /Loop1/                 l+ 0
-- /inLoop1/               l+ 0
-- /inIfinLoop1/           l+ 0
-- /postLoop1/             l+ 0
-- /Loop2/                 l+ 0
-- /inLoop2/               l+ 0
-- /inIfinLoop2/           l+ 0
-- /elab/                  l+ 0
