pragma Ada_2012;

with Pkg;

procedure Test_1 is
begin
   Pkg.Ignore_1 (Pkg.My_String'Access);
end Test_1;

--# pkg.ads
-- /p-1/ l+ ## 0
-- /p-2/ l- ## s-
