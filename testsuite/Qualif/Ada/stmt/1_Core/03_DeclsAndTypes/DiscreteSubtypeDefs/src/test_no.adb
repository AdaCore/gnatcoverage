--  Test driver for discrete subtype definitions. It only 'with's the
--  functional code but does not call or instantiate anything from it, so only
--  the library-level declaration is expected to be reported as covered.

with Defs;
with Support; use Support;

procedure Test_No is
begin
   Assert (True);
end;
--# defs.ads
-- /dcl/    l+ ## 0

-- %opts: --trace-mode=src
-- /g1_dcl/ l- ## s-
-- /g2_dcl/ l- ## s-

--# defs.adb
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




