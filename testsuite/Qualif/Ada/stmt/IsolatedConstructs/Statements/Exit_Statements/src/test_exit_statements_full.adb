--  Test driver for EXIT statements. It executes all the functional code such
--  as everything is expected to be reported as covered.

with EXIT_Statements_Support; use EXIT_Statements_Support;
with EXIT_Statements;      use EXIT_Statements;
with Support;              use Support;

procedure Test_EXIT_Statements_Full is

   function My_Factorial is new Factorial (Integer);

   Test_Array_Sample : Array_Sample :=
     (1 => Identity (1),
      2 => Identity (2),
      3 => Identity (3));

   Test_Matrix_Sample : Matrix_Sample :=
     (1 => (1 => Identity (1),
            2 => Identity (2)),
      2 => (1 => Identity (3),
            2 => Identity (4)));

   Res : Integer;
begin
   Assert (My_Factorial (Identity (3)) = 6);

   Update_Array_Sample (Test_Array_Sample, 4);

   Assert (Test_Array_Sample (1) = 1 and then
           Test_Array_Sample (2) = 3 and then
           Test_Array_Sample (3) = 3);

   Res := Compute_On_Matrix (Test_Matrix_Sample, 5);
   Assert (Res = 6);
end Test_EXIT_Statements_Full;

--# exit_statements.adb
-- /1preloop/         l+ 0
-- /1loop/            ~l+ ~0
-- /in1loop1exit/     l+ 0
-- /in1loop2exit/     l+ 0
-- /in1loopafterexit/ l+ 0
-- /post1loop/        l+ 0

-- /2preloop/          l+ 0
-- /2loop/             l+ 0
-- /21exit/            l+ 0
-- /2after1exit/       l+ 0
-- /22exit/            l+ 0
-- /2after2exit/       l+ 0

-- /3preloop/          l+ 0
-- /3outerloop/        l+ 0
-- /3innerloop/        l+ 0
-- /31exit/            l+ 0
-- /3after1exit/       l+ 0
-- /32exit/            l+ 0
-- /3after2exit/       l+ 0
-- /3afterinnerloop/   l+ 0
-- /postloop/          l+ 0
