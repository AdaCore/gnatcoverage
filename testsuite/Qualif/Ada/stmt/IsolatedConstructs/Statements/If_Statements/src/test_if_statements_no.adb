--  Test driver for IF statements. It only "with"s the functional code,
--  but does not execute anything from it, so no IF statements are expected to
--  be reported as covered.

with If_Statements; use If_Statements;
with Support;       use Support;
procedure Test_IF_Statements_No is
   procedure My_Set_Max is new Set_Max (Integer);
begin
   Assert (True);
end Test_IF_Statements_No;

--# if_statements.adb
-- /XcmpMin/ l- s-
-- /XoutMin/ l- s-
-- /XcmpMax/ l- s-
-- /XoutMax/ l- s-
-- /Xin/ l- s-
-- /setmax/ ~l- ~s-
-- /inifsetmax/ ~l- ~s-
