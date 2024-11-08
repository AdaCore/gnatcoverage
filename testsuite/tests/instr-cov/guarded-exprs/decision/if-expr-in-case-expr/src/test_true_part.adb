with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_True_Part is
begin
   Put_Line (If_Expr_In_Case_Expr (True, True));
end Test_True_Part;

--# pkg.adb
-- /return/       l+ ## 0
-- /case_true/    l+ ## 0
-- /if_true/      l! ## dF-
-- /case_false/   l! ## g-
