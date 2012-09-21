
--  Test driver for LOOP statements. Don't even call into the functional
--  code. Expect everything uncovered except the package elaboration parts.

with LOOP_Statements;      use LOOP_Statements;
with More_LOOP_Statements; use More_LOOP_Statements;
with Instances, Support;   use Instances, Support;

procedure Test_No_Execution is
begin
   Assert (True);
end Test_No_Execution;

--# loop_statements.adb
-- /preLoop1/              l- ## s-
-- /loop1/                 l- ## s-
-- /inloopbeforeexit1/     l- ## s-
-- /inloopexit1/           l- ## s-
-- /inloopafterexit1/      l- ## s-
-- /postLoop1/             l- ## s-
-- /preLoop2/              l- ## s-
-- /loop2/                 l- ## s-
-- /inloopbeforeexit2/     l- ## s-
-- /inloopbeforeexitinif2/ l- ## s-
-- /inloopexit2/           l- ## s-
-- /preLoop3/              l- ## s-
-- /inloopexit3/           l- ## s-
-- /inloop3/               l- ## s-
-- /postLoop3/             l- ## s-
-- /preLoop4/              l- ## s-
-- /loop4/                 l- ## s-
-- /inloopexit4/           l- ## s-
-- /inloop4/               l- ## s-

--# more_loop_statements.adb
-- /preLoop1/              l- ## s-
-- /Loop1/                 l- ## s-
-- /inLoop1/               l- ## s-
-- /inIfinLoop1/           l- ## s-
-- /postLoop1/             l- ## s-
-- /Loop2/                 l- ## s-
-- /inLoop2/               l- ## s-
-- /inIfinLoop2/           l- ## s-
-- /elab/                  l+ ## 0
