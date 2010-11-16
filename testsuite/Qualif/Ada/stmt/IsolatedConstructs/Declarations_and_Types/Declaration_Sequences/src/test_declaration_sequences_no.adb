--  Test driver for declaration sequences. It only 'with's the functional code
--  but does not call or instantiate anything from it, so only the
--  library-level declaration sequence is expected to be reported as covered.

with Decls_Pack;
with Support; use Support;

procedure Test_DEclaration_Sequences_No is
begin
   Assert (True);
end Test_DEclaration_Sequences_No;

--# decls_pack.adb

--  /lib_level_dcl/ l+ 0
--  /gen_dcl/       l- s-
--  /local_dcl/     l- s-
--  /stmt/          l- s-
