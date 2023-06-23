with Support;

with Pkg; use Pkg;

procedure Test_Elsif is
begin
   Support.Assert (Or_Else (False, True));
end Test_Elsif;

--# pkg.adb
--
-- /if-stmt/    l+ ## 0
-- /ret-if/     l- ## s-
-- /elsif-stmt/ l+ ## 0
-- /ret-elsif/  l+ ## 0
-- /ret-else/   l- ## s-
