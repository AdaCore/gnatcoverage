--  Test driver for declaration sequences. It calls a subprogram from the
--  functional code to get the local declarations elaborated, so declarations
--  from a local subprogram and library-level declarations are expected to be
--  reported as covered, and declarations from generic package (that is not
--  instantiated) as uncovered.

with Decls_Support; use Decls_Support;
with Decls_Pack;    use Decls_Pack;
with Support;       use Support;

procedure Test_Declaration_Sequences_Local is
   My_Vector        : Vector (1 .. 3) := (1, 2, 3);
   My_Sorted_Vector : Vector (My_Vector'Range);
begin
   My_Sorted_Vector := Simple_Sort (My_Vector);
   Assert (My_Sorted_Vector(1) = 3
       and then My_Sorted_Vector(2) = 2
       and then My_Sorted_Vector(3) = 1);
end Test_Declaration_Sequences_Local;

--# decls_pack.ads
--  /lib_level_dcl/ l+ 0
--  /gen_dcl/       ~l- ~s-

--# decls_pack.adb

--  /local_dcl/     l+ 0
--  /stmt/          l+ 0
