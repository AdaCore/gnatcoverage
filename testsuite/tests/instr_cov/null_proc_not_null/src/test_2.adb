pragma Ada_2012;

with Pkg;

procedure Test_2 is
begin
   Pkg.Ignore_2 (Pkg.My_String'Access);
end Test_2;

--# pkg.ads
-- /p-1/ l- ## s-
-- /p-2/ l+ ## 0
