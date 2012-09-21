
with Support, Expr; use Support, Expr;

procedure Test_Expr_Full is
begin
   -- cover and-then
   
   If_Andthen (True, False, 5);
   Assert (Expr.Value = 0);
   
   If_Andthen (False, True, 6);
   Assert (Expr.Value = 0);
   
   If_Andthen (False, False, 7);
   Assert (Expr.Value = 0);
   
   If_Andthen (True, True, 9);
   Assert (Expr.Value = 9);
   
   -- cover or-else
   
   If_Orelse (True, False, 5);
   Assert (Expr.Value = 5);
   
   If_Orelse (False, True, 6);
   Assert (Expr.Value = 6);
   
   If_Orelse (False, False, 7);
   Assert (Expr.Value = 6);
   
   If_Orelse (True, True, 9);
   Assert (Expr.Value = 9);
end;

--# expr.adb
--  /eval/  l+ ## 0
--  /latch/ l+ ## 0
