--  Test driver for CASE statements. It only "with"s the functional code,
--  but does not execute anything from it, so everything is expected to be
--  reported as uncovered.

with CASE_Statements; use CASE_Statements;
with Support;         use Support;
procedure Test_No_Execution is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 1, 2);
begin
   Assert (True);
end Test_No_Execution;

--# case_statements.adb
-- /colorcase/ l- s-
-- /white/     l- s-
-- /red/       l- s-
-- /yellow/    l- s-
-- /green/     l- s-
-- /blue/      l- s-
-- /brown/     l- s-
-- /black/     l- s-
-- /valcase/  ~l- ~s-
-- /1case/    ~l- ~s-
-- /2case/    ~l- ~s-
-- /4case/    ~l- ~s-
-- /7case/    ~l- ~s-
-- /others/   ~l- ~s-
