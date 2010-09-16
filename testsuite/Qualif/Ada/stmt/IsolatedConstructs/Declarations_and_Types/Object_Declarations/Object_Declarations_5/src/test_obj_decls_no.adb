--  Test driver for object declarations. It only "with"s the functional
--  subprogram Swap that contains declaration of interest, but does not call
--  it, so nothing is expected to be reported as covered.

with Swap;
with Support; use Support;

procedure Test_Obj_Decls_No is
begin
   Assert (True);
end Test_Obj_Decls_No;

--# swap.adb
-- /stmt/ l- s-
-- /if/   l- s-
