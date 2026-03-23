--  Test driver for subprogram renamings. It executes all the functional code.
--  Nothing is expected to be reported as uncovered.

with Pack; use Pack;
with Support;         use Support;
procedure Test_Subunits_Full is
   Int : Integer;
begin
   Assert (Inner.Fun (1) = 2);
   Assert (Inner.Fun (0) = 0);

   Assert (New_Value (0) = 1);

   Int := 2;
   Non_Separate_Proc (Int);
   Assert (Int = 1);

   Update (Int);
   Assert (Int = 2);
end Test_Subunits_Full;

--# pack-inner-fun-proc.adb
-- /stmt/     l+ ## 0

--# pack-inner-fun.adb
-- /stmt/     l+ ## 0
-- /if/       l+ ## 0
-- /else/     l+ ## 0

--# pack-inner.adb
-- /stmt/     l+ ## 0

--# pack-new_value.adb
-- /stmt/     l+ ## 0

--# pack-update.adb
-- /stmt/     l+ ## 0

--# pack.adb
-- /stmt/     l+ ## 0
