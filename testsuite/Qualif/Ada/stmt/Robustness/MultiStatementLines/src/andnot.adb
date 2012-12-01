procedure Andnot (A, B : Boolean; E : out Boolean) is
   Not_B : Boolean; -- # decl
begin
   --  Straight sequence of statements on a single line, without
   --  conditional control here.
   
   Not_B := not B; E := A and then Not_B;  -- # doAndNot
end;
