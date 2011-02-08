--  Test driver for declaration sequences It makes all the declaration
--  sequences from the functional code elaborated, so nothing is expected to
--  be reported as uncovered

with Decls_Support; use Decls_Support;
with Decls_Pack;    use Decls_Pack;
with Support;       use Support;

procedure Test_Declaration_Sequences_All is
   package Inner is new Inner_G (13);
   My_Vector        : Vector (1 .. 3) := (1, 2, 3);
   My_Sorted_Vector : Vector (My_Vector'Range);
begin
   Assert (Inner.Int = 13);

   My_Sorted_Vector := Simple_Sort (My_Vector);
   Assert (My_Sorted_Vector(1) = 3
       and then My_Sorted_Vector(2) = 2
       and then My_Sorted_Vector(3) = 1);
end Test_Declaration_Sequences_All;

--# decls_pack.ads
--  /lib_level_dcl/ l+ 0
--  /gen_dcl/       l+ 0

--# decls_pack.adb

--  /local_dcl/     l+ 0
--  /stmt/          l+ 0
