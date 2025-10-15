with Disabled; use Disabled;
with Support;  use Support;

procedure Test_All is
begin
   Assert (Complex_Identity (0) = 0);
   Assert (Complex_Identity (2) = 2);
   Assert (Complex_Identity (6) = 6);
end Test_All;

--# disabled.adb
--  /disabled/ lD ## 0
--  /dis-ex/   lD ## x0
--  /exempted/ l# ## x0
