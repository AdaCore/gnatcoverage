pragma Ada_2012;

with Pkg;

procedure Test_12 is
begin
   Pkg.Ignore_1 (Pkg.My_String'Access);
   Pkg.Ignore_2 (Pkg.My_String'Access);
end Test_12;

--# pkg.ads
-- /p-1/ l+ ## 0
-- /p-2/ l+ ## 0
