with Pkg; use Pkg;

--  Check the correct function coverage analysis of different kinds of null
--  procedures when called.

procedure Test_Called is
begin
    P1;
    P2 (1, 2);
    P3;
end Test_Called;

--# pkg.ads
-- /p/ l+ ## 0

--# pkg.adb
-- /p/ l+ ## 0
