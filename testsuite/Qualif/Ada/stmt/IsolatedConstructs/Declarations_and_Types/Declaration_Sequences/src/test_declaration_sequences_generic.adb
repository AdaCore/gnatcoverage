--  Test driver for declaration sequences. It instantiates a generic unit
--  from the functional code, so declarations from a local subprogram are
--  expected to be reported as uncovered, and all the other declarations - as
--  covered.

with Decls_Pack; use Decls_Pack;
with Support; use Support;

procedure Test_DEclaration_Sequences_Generic is
   package Inner is new Inner_G (13);
begin
   Assert (Inner.Int = 13);
end Test_DEclaration_Sequences_Generic;

--# decls_pack.adb

--  /lib_level_dcl/ l+ 0
--  /gen_dcl/       l+ 0
--  /local_dcl/     l- s-
--  /stmt/          l- s-
