with Assert, Pkg; use Pkg;

procedure Test_DT is
begin
   Assert (Foo (abc));
end;

--# pkg.adb
-- /evalA/  s=>l+, dmu=>l! ## s=>0, dmu=>dF-
-- /evalB/  s=>l+, dmu=>l! ## 0
-- /evalC/  s=>l+, dmu=>l! ## 0
-- /dtrue/  l+ ## 0
-- /dfalse/ l- ## s-
