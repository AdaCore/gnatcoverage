--  Test driver for discrete subtype definitions. It only 'with's the
--  functional code but does not call or instantiate anything from it, so only
--  the library-level declaration is expected to be reported as covered.

with Discrete_Subtype_Defs;
with Support; use Support;

procedure Test_Discrete_Subtype_Defs_No is
begin
   Assert (True);
end Test_Discrete_Subtype_Defs_No;

--# discrete_subtype_defs.ads
-- /dcl/    l+ ## 0
-- /g1_dcl/ ~l- ## ~s-
-- /g1_dcl/ ~l- ## ~s-

--# discrete_subtype_defs.adb
-- /1_local_dcl/ l- ## s-
-- /1_stmt/      l- ## s-
-- /1_1_loop/    l- ## s-
-- /1_2_loop/    l- ## s-
-- /1_if/        l- ## s-

-- /2_local_dcl/ l- ## s-
-- /2_stmt/      l- ## s-
-- /2_1_loop/    l- ## s-
-- /2_2_loop/    l- ## s-
-- /2_if/        l- ## s-




