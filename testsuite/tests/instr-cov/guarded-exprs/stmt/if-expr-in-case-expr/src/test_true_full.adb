with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_True_Full is
begin
   Put_Line (If_Expr_In_Case_Expr (True, True));
   Put_Line (If_Expr_In_Case_Expr (True, False));
end Test_True_Full;

--# pkg.adb
-- /case_false/   l! ## g-
