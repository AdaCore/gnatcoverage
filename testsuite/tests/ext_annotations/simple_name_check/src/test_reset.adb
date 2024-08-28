with Reset;

with Support;

procedure Test_Reset is
   X : Integer := 10;
begin
   Reset (X);
   Support.Assert (X = 0);
end Test_Reset;

--# reset.adb
--
-- /st/ l# ## x0
