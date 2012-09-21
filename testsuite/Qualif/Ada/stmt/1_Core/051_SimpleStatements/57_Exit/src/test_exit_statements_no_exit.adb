--  Test driver for EXIT statements. It executes parts of the functional code
--  that contains conditional loop statements with at least one EXIT statement,
--  and loop condition prevent loop even the first iteration for each of these
--  loops. So all the EXIT statements are expected to be reported as uncovered.

with EXIT_Statements_Support; use EXIT_Statements_Support;
with EXIT_Statements;         use EXIT_Statements;
with Support;                 use Support;
procedure Test_EXIT_Statements_No_Exit is
   function My_Factorial is new Factorial (Integer);

   Null_Array_Sample : Array_Sample (1 .. 0) := (others => Identity (1));

   Null_Matrix_Sample : Matrix_Sample (1 .. 0, 1 .. 2) :=
     (others => (others => Identity (11)));

   Res : Integer;
begin

   Update_Array_Sample (Null_Array_Sample, 10);
   Assert (Null_Array_Sample'Length = 0);

   Res := Compute_On_Matrix (Null_Matrix_Sample, 1000);
   Assert (Res = 0);
end Test_EXIT_Statements_No_Exit;

--# exit_statements.adb
-- /1preloop/         ~l- ## ~s-
-- /1loop/            ~l- ## ~s-
-- /in1loop1exit/     ~l- ## ~s-
-- /in1loop2exit/     ~l- ## ~s-
-- /in1loopafterexit/ ~l- ## ~s-
-- /post1loop/        ~l- ## ~s-

-- /2preloop/          l+ ## 0
-- /2loop/             l+ ## 0
-- /21exit/            l- ## s-
-- /2after1exit/       l- ## s-
-- /22exit/            l- ## s-
-- /2after2exit/       l- ## s-

-- /3preloop/          l+ ## 0
-- /3outerloop/        l+ ## 0
-- /3innerloop/        l- ## s-
-- /31exit/            l- ## s-
-- /3after1exit/       l- ## s-
-- /32exit/            l- ## s-
-- /3after2exit/       l- ## s-
-- /3afterinnerloop/   l- ## s-
-- /postloop/          l+ ## 0
