with Support, Vectors; use Support, Vectors;
                
procedure Test_Vector_Pos is
begin
   Assert (Sum_All_Abs (N => 5, Value => 3) = 15);
end;

--# vectors.adb
--  /on-call/  l+ ## 0
--  /pos-decl/ l+ ## 0
--  /pos-stmt/ l+ ## 0
--  /neg-decl/ l- ## s-
--  /neg-stmt/ l- ## s-

