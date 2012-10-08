with Support, Vectors; use Support, Vectors;
                
procedure Test_Vector_Neg is
begin
   Assert (Sum_All_Abs (N => 7, Value => -4) = 28);
end;

--# vectors.adb
--  /on-call/  l+ ## 0
--  /pos-decl/ l- ## s-
--  /pos-stmt/ l- ## s-
--  /neg-decl/ l+ ## 0
--  /neg-stmt/ l+ ## 0

