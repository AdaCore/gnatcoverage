with Support;

with Pkg; use Pkg;

procedure Test_No is
begin
   null;
end Test_No;

--# pkg.adb
--
-- /.*-stmt/ l- ## s-
-- /ret-.*/  l- ## s-
