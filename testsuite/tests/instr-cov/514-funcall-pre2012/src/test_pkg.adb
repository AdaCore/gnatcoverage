with Pkg;

procedure Test_Pkg is
begin
   Pkg.Print (False);
   Pkg.Print (True);
end Test_Pkg;

--# pkg.adb
-- /call/ l? ## c?
-- /stmt/ l+ ## 0
