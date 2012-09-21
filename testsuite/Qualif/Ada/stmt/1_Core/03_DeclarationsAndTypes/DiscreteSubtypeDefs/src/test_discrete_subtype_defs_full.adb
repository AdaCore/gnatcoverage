--  Test driver for discrete subtype definitions. It instantiates all the
--  genrics from the functional code and calls part all the subprograms from
--  it. So all the declarations are expected to be reported as covered.

with Discrete_Subtype_Defs; use Discrete_Subtype_Defs;
with Support;                      use Support;

procedure Test_Discrete_Subtype_Defs_Full is
   package P1 is new P1_G (10);
   package P2 is new P2_G (20);
begin
   Assert (P1.Arr'Length = 10);
   Assert (P2.Arr'Length = 20);

   Assert (Some_Fun_1 (1, 2, 3, -10));
   Assert (not Some_Fun_2 (1, 2, 3, 100));
end Test_Discrete_Subtype_Defs_Full;

--# discrete_subtype_defs.ads
-- /dcl/    l+ ## 0
-- /g1_dcl/ l+ ## 0
-- /g1_dcl/ l+ ## 0

--# discrete_subtype_defs.adb
-- /1_local_dcl/ l+ ## 0
-- /1_stmt/      l+ ## 0
-- /1_1_loop/    l+ ## 0
-- /1_2_loop/    l+ ## 0
-- /1_if/        l- ## s-

-- /2_local_dcl/ l+ ## 0
-- /2_stmt/      l+ ## 0
-- /2_1_loop/    l+ ## 0
-- /2_2_loop/    l+ ## 0
-- /2_if/        l+ ## 0
