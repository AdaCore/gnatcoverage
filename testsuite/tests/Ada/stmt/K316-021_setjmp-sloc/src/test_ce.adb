with Plop, Support; use Plop, Support;

procedure Test_CE is
begin
   Assert (F (50) = -1);
end;

--# plop.adb
-- /mult/    l+ ## 0
-- /div/     l- ## s-
-- /retval/  l- ## s-
-- /handle/  l+ ## 0
