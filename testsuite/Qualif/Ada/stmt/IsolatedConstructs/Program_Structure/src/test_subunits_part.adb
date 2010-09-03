--  Test driver for subprogram renamings. It executes a part of the functional
--  code.

with Pack; use Pack;
with Support;         use Support;
procedure Test_Subunits_Part is
begin
   Assert (Inner.Fun (1) = 2);
end Test_Subunits_Part;

--# pack-inner-fun-proc.adb
-- /stmt/     l- s-

--# pack-inner-fun.adb
-- /stmt/     l+ 0
-- /if/       l+ 0
-- /else/     l- s-

--# pack-inner.adb
-- /stmt/     l+ 0

--# pack-new_value.adb
-- /stmt/     l- s-

--# pack-update.adb
-- /stmt/     l- s-

--# pack.adb
-- /stmt/     l- s-
