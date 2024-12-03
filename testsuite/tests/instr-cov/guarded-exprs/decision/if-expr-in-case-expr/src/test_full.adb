with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Test_Full is
begin
   Put_Line (If_Expr_In_Case_Expr (True, True));
   Put_Line (If_Expr_In_Case_Expr (True, False));
   Put_Line (If_Expr_In_Case_Expr (False, True));
   Put_Line (If_Expr_In_Case_Expr (False, False));
end Test_Full;

--# pkg.adb
-- /ret/          l+ ## 0
-- /case_true/    l+ ## 0
-- /if_true/      l+ ## 0
-- /case_false/   l+ ## 0
