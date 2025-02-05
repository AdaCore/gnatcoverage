with Pkg; use Pkg;

procedure Test_Called is
begin
    P1;
    P2 (1, 2);
    P3;
end Test_Called;

--# pkg.adb
-- /p/ l+ ## 0
--# pkg.ads
-- /p/ l+ ## 0
