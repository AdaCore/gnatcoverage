--  Test driver for discrete subtype definitions. It instantiates part of the
--  genrics from the functional code and calls part of the subprograms from it.
--  (Test_Discrete_Subtype_Defs_Part_2 does this for another part). So
--  the library-level declaration and some of the declarations from generics
--  and local bodies ae expected to be reported as covered, and other
--  declarations are expected to be reported as uncovered.

with Defs; use Defs;
with Support;                      use Support;

procedure Test_Part_1 is
   package P1 is new P1_G (10);
begin
   Assert (P1.Arr'Length = 10);

   Assert (Some_Fun_1 (1, 2, 3, -10));
end;
--# defs.ads
-- /dcl/    l+ ## 0
-- /g1_dcl/ l+ ## 0

-- %opts: --trace-mode=src
-- /g2_dcl/ l- ## s-


--# defs.adb
-- /1_local_dcl/ l+ ## 0
-- /1_stmt/      l+ ## 0
-- /1_1_loop/    l+ ## 0
-- /1_2_loop/    l+ ## 0
-- /1_if/        l- ## s-

-- /2_local_dcl/ l- ## s-
-- /2_stmt/      l- ## s-
-- /2_1_loop/    l- ## s-
-- /2_2_loop/    l- ## s-
-- /2_if/        l- ## s-
