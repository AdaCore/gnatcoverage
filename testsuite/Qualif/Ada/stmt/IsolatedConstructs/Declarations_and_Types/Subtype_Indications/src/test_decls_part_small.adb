--  Test driver for declarations. The goal is to have not all but a "small"
--  part of constructs of interest covered.
--
--  The driver only "with"s the functional package Decls_Pack that contains
--  declarations of interest, but does not call local subprograms from it, so
--  only global declarations from the package should be reported as covered

with Decls_Pack;
with Support; use Support;

procedure Test_Decls_Part_Small is
begin
   Assert (True);
end Test_Decls_Part_Small;

--# decls_pack.adb
--  /code1/ l- s-
--  /decl1/ ~l- s-
--  /code2/ l- s-
--  /decl2/ ~l- s-

--# decls_pack.ads
--  /dcls/ ~l+ 0
