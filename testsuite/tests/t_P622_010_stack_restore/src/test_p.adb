with Characterize;

procedure Test_P is
begin
   Characterize (1);
end Test_P;

--# characterize.adb
--  /decl/        l+ ## 0
--  /first-cond/  l! ## dF-
--  /first-stmt/  l+ ## 0
--  /second-cond/ l- ## s-
--  /second-stmt/ l- ## s-
--  /third-stmt/  l- ## s-
