--  Don't exercise any of the functional code at all. Expect everything to be
--  reported uncovered except the relevant package elaboration body parts.

with If_Statements;      use If_Statements;
with More_IF_Statements; use More_IF_Statements;
with Instances, Support; use Instances, Support;

procedure Test_No_Execution is
begin
   null;
end Test_No_Execution;

--# if_statements.adb
-- /XcmpMin/     l- s-
-- /XoutMin/     l- s-
-- /XcmpMax/     l- s-
-- /XoutMax/     l- s-
-- /Xin/         l- s-
-- /setmax/      l- s-
-- /inifsetmax/  l- s-

--# more_if_statements.adb
-- /prime/       l- s-
-- /ifprime/     l- s-
-- /1prime/      l- s-
-- /comp2prime/  l- s-
-- /2prime/      l- s-
-- /comp3prime/  l- s-
-- /3prime/      l- s-
-- /comp4prime/  l- s-
-- /4prime/      l- s-
-- /comp5prime/  l- s-
-- /5prime/      l- s-
-- /comp6prime/  l- s-
-- /6prime/      l- s-
-- /comp7prime/  l- s-
-- /7prime/      l- s-
-- /comp8prime/  l- s-
-- /8prime/      l- s-
-- /comp9prime/  l- s-
-- /9prime/      l- s-
-- /comp10prime/ l- s-
-- /10prime/     l- s-
-- /max/         l- s-
-- /ifmax/       l- s-
--  /elsemax/    l- s-
-- /elab/        l+ 0
-- /gt0elab/     l+ 0
-- /notgt0elab/  l- s-
-- /eq0elabeq0/  l- s-
