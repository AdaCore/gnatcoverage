--  Test driver for discrete subtype definitions. It instantiates part of the
--  genrics from the functional code and calls part of the subprograms from it.
--  (Test_Discrete_Subtype_Definitions_Part_1 does this for another part). So
--  the library-level declaration and some of the declarations from generics
--  and local bodies ae expected to be reported as covered, and other
--  declarations are expected to be reported as uncovered.

with Discrete_Subtype_Definitions; use Discrete_Subtype_Definitions;
with Support;                      use Support;

procedure Test_Discrete_Subtype_Definitions_Part_2 is
   package P2 is new P2_G (20);
begin
   Assert (P2.Arr'Length = 20);

   Assert (not Some_Fun_2 (1, 2, 3, 100));
end Test_Discrete_Subtype_Definitions_Part_2;

--# discrete_subtype_definitions.ads
-- /dcl/    l+ 0
-- /g1_dcl/ ~l- ~s-
-- /g2_dcl/ l+ 0

--# discrete_subtype_definitions.adb
-- /1_local_dcl/ l- s-
-- /1_stmt/      l- s-
-- /1_1_loop/    l- s-
-- /1_2_loop/    l- s-
-- /1_if/        l- s-

-- /2_local_dcl/ l+ 0
-- /2_stmt/      l+ 0
-- /2_1_loop/    l+ 0
-- /2_2_loop/    l+ 0
-- /2_if/        l+ 0
