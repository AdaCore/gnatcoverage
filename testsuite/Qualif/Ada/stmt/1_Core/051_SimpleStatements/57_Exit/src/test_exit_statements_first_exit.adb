--  Test driver for EXIT statements. It executes parts of the functional code
--  that contain loop statements with at least one EXIT statement, and this
--  EXIT statement is executed and transfers the control out of the loop at the
--  first loop iteration (in case when the loop contains more then one EXIT
--  statement, the first one exits from the loop at the first iteration).
--  Therefore, all the statements in the loops down to this EXIT statements
--  (including these exits) are expected to be reported as covered, and all the
--  statements in these loops following these EXIT statements are expected to
--  be reported as non covered.

with EXIT_Statements_Support; use EXIT_Statements_Support;
with EXIT_Statements;         use EXIT_Statements;
with Support;                 use Support;
procedure Test_EXIT_Statements_First_Exit is
   function My_Factorial is new Factorial (Integer);

   Test_Array_Sample : Array_Sample :=
     (1 => Identity (-1),
      2 => Identity (0));

   Test_Matrix_Sample : Matrix_Sample (1 .. 1, 1 .. 2) :=
     (others => (others => Identity (-11)));

   Res : Integer;
begin
   Assert (My_Factorial (Identity (-1)) = 1);

   Update_Array_Sample (Test_Array_Sample, 10);
   Assert (Test_Array_Sample (1) = -1 and then
           Test_Array_Sample (2) = 0);

   Res := Compute_On_Matrix (Test_Matrix_Sample, 1000);
   Assert (Res = 0);
end Test_EXIT_Statements_First_Exit;

--# exit_statements.adb
-- /1preloop/         l+ ## 0
-- /1loop/            ~l- ## ~s-
-- /in1loop1exit/     l+ ## 0
-- /in1loop2exit/     l- ## s-
-- /in1loopafterexit/ l- ## s-
-- /post1loop/        l+ ## 0

-- /2preloop/          l+ ## 0
-- /2loop/             l+ ## 0
-- /21exit/            l+ ## 0
-- /2after1exit/       l- ## s-
-- /22exit/            l- ## s-
-- /2after2exit/       l- ## s-

-- /3preloop/          l+ ## 0
-- /3outerloop/        l+ ## 0
-- /3innerloop/        l+ ## 0
-- /31exit/            l+ ## 0
-- /3after1exit/       l- ## s-
-- /32exit/            l- ## s-
-- /3after2exit/       l- ## s-
-- /3afterinnerloop/   l- ## s-
-- /postloop/          l+ ## 0
