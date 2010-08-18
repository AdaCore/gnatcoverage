--  Test driver for declarations. It only "with"s the functional subprogram
--  Decls that contains declarations of interest, but does not call it, so
--  nothing from the constructs of interest is expected to be reported as
--  covered.

with Decls;
with Support; use Support;

procedure Test_Decls_No is
begin
   Assert (True);
end Test_Decls_No;

--# decls.adb
--  /dcls/ l- s-
--  /stmt/ l- s-
