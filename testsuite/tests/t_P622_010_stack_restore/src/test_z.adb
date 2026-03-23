with Characterize;

procedure Test_Z is
begin
   Characterize (0);
end Test_Z;

--# characterize.adb
--  /decl/        l+ ## 0
--  /first-cond/  l! ## dT-
--  /first-stmt/  l- ## s-
--  /second-cond/ l! ## dT-
--  /second-stmt/ l- ## s-
--  /third-stmt/  l+ ## 0
