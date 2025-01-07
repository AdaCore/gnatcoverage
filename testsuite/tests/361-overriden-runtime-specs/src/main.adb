with Ada.Containers.Vectors;

procedure Main is
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);

   V : Int_Vectors.Vector;
begin
   V.Append (3);
end Main;
