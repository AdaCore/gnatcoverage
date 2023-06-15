with Characterize;

procedure Test_N is
begin
   Characterize (-1);
end Test_N;

--# characterize.adb
--  /decl/        l+ ## 0
--  /first-cond/  l! ## dT-
--  /first-stmt/  l- ## s-
--  /second-cond/ l! ## dF-
--  /second-stmt/ l+ ## 0
--  /third-stmt/  l- ## s-

