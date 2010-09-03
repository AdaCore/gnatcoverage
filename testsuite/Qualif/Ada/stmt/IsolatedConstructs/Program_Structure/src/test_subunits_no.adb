--  Test driver for subprogram renamings. It only "with's" the functional code
--  but does not execute anything from it. So nothing is expected to be
--  reported as covered except the package elaboration code.

with Pack; use Pack;
with Support;         use Support;
procedure Test_Subunits_No is
begin
   Assert (True);
end Test_Subunits_No;

--# pack-inner-fun-proc.adb
-- /stmt/     l- s-

--# pack-inner-fun.adb
-- /stmt/     l- s-
-- /if/       l- s-
-- /else/     l- s-

--# pack-inner.adb
-- /stmt/     l+ 0

--# pack-new_value.adb
-- /stmt/     l- s-

--# pack-update.adb
-- /stmt/     l- s-

--# pack.adb
-- /stmt/     l- s-
