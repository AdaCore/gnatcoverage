with Support; use Support;
with Maps; use Maps;

-- This is a unit test for the Maps unit

procedure Test_Area is
   R : Rectangle_T := (L => 3, W => 9);
begin
   Assert (Area (R) = 27);
end;

-- No expectation on the Math unit here

--# maps.adb
--  /eval/ l+ ## 0

