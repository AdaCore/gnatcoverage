pragma Ada_2012;

with Pkg;

procedure Test_No is
begin
   Pkg.My_String (Pkg.My_String'First) := 'H';
end Test_No;

--# pkg.ads
-- /p-1/ l- ## s-
-- /p-2/ l- ## s-
