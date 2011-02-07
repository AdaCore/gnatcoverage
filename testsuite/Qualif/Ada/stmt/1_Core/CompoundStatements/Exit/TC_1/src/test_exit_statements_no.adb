--  Test driver for EXIT statements. It only "with"s the functional code,
--  but does not execute anything from it, so all the EXIT statements are
--  expected to be reported as uncovered.

with EXIT_Statements;      use EXIT_Statements;
with Support;              use Support;
procedure Test_EXIT_Statements_No is
   function My_Factorial is new Factorial (Natural);
begin
   Assert (True);
end Test_EXIT_Statements_No;

--# exit_statements.adb
-- /1preloop/         ~l- ~s-
-- /1loop/            ~l- ~s-
-- /in1loop1exit/     ~l- ~s-
-- /in1loop2exit/     ~l- ~s-
-- /in1loopafterexit/ ~l- ~s-
-- /post1loop/        ~l- ~s-

-- /2preloop/          l- s-
-- /2loop/             l- s-
-- /21exit/            l- s-
-- /2after1exit/       l- s-
-- /22exit/            l- s-
-- /2after2exit/       l- s-

-- /3preloop/          l- s-
-- /3outerloop/        l- s-
-- /3innerloop/        l- s-
-- /31exit/            l- s-
-- /3after1exit/       l- s-
-- /32exit/            l- s-
-- /3after2exit/       l- s-
-- /3afterinnerloop/   l- s-
-- /postloop/          l- s-
