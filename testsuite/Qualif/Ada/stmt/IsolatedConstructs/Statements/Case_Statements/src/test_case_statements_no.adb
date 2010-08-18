--  Test driver for IF statements. It only "with"s the functional code,
--  but does not execute anything from it, so no IF statements are expected to
--  be reported as covered.

with CASE_Statements; use CASE_Statements;
with Support;       use Support;
procedure Test_CASE_Statements_No is
   procedure My_Adjust_Int is new Adjust_Int_P (Integer, 1, 2);
begin
   Assert (True);
end Test_CASE_Statements_No;

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
