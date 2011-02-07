--  Test driver for EXIT statements. It executes parts of the functional code
--  that contain loop statements with two or more EXIT statements, and some of
--  these EXIT statement, but not the first in the loop body is executed and
--  transfers the control out of the loop at the first loop iteration
--  Therefore, all the statements in the loops down to this EXIT statements
--  (including these exits) are expected to be reported as covered, and all the
--  statements in these loops following these EXIT statements are expected to
--  be reported as uncovered. Note, that first EXIT statements in loop bodies
--  are expected to be reported as covered.

with EXIT_Statements_Support; use EXIT_Statements_Support;
with EXIT_Statements;      use EXIT_Statements;
with Support;              use Support;
procedure Test_EXIT_Statements_Second_Exit is
   function My_Factorial is new Factorial (Integer);
   Test_Array_Sample : Array_Sample :=
     (1 => Identity (100),
      2 => Identity (10));

   Test_Matrix_Sample : Matrix_Sample :=
     (1 => (1 => Identity (100),
            2 => Identity (1)),
      2 => (1 => Identity (10),
            2 => Identity (20)));

   Res : Integer;
begin
   Update_Array_Sample (Test_Array_Sample, 10);
   Assert (Test_Array_Sample (1) = 100 and then
           Test_Array_Sample (2) = 10);

   Res := Compute_On_Matrix (Test_Matrix_Sample, 10);
   Assert (Res = 100);
end Test_EXIT_Statements_Second_Exit;

--# exit_statements.adb
-- /1preloop/         ~l- ~s-
-- /1loop/            ~l- ~s-
-- /in1loop1exit/     ~l- ~s-
-- /in1loop2exit/     ~l- ~s-
-- /in1loopafterexit/ ~l- ~s-
-- /post1loop/        ~l- ~s-

-- /2preloop/          l+ 0
-- /2loop/             l+ 0
-- /21exit/            l+ 0
-- /2after1exit/       l+ 0
-- /22exit/            l+ 0
-- /2after2exit/       l- s-

-- /3preloop/          l+ 0
-- /3outerloop/        l+ 0
-- /3innerloop/        l+ 0
-- /31exit/            l+ 0
-- /3after1exit/       l+ 0
-- /32exit/            l+ 0
-- /3after2exit/       l- s-
-- /3afterinnerloop/   l- s-
-- /postloop/          l+ 0
