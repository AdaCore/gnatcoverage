-- test_std.adb

with Std;
with Support; use Support;

procedure Test_Std is
   X : Integer := 12;
begin
   Std.Process (X);
   Assert (X = 12 + 15);
end;

--# std.adb
-- /eval/ l! ## dF-
