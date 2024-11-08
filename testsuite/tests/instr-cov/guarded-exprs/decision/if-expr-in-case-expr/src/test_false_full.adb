with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_False_Full is
begin
   Put_Line (If_Expr_In_Case_Expr (False, True));
   Put_Line (If_Expr_In_Case_Expr (False, False));
end Test_False_Full;

--# pkg.adb
-- /case_true/    l! ## g-
-- /if_true/      l! ## d-
