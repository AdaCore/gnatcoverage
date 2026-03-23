with Math;
with Vectors;

procedure Main is
   package Number_Vectors is new Vectors (Math.Number, 10);

   use Math, Number_Vectors;

   V : Vector;
begin
   Push (V, From_Integer (1));
   Push (V, From_Integer (2));
   Push (V, Element (V, 1) + Element (V, Last_Index (V)));
end Main;
