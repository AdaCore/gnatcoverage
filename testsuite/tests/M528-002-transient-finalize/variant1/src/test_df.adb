with Assert, Pkg; use Pkg;

procedure Test_DF is   
begin
   Assert (not Foo (bool));
end;

--# pkg.adb
-- /evalA/  s=>l+, dmu=>l! ## s=>0, dmu=>dT-
-- /evalB/  s=>l+, dmu=>l! ## 0
-- /evalC/  s=>l+, dmu=>l! ## 0
-- /dtrue/  l- ## s-
-- /dfalse/ l+ ## 0


