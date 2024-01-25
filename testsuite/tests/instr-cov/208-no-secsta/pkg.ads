package Pkg is

   type Int_Range is record
      Low_Bound, High_Bound : Integer;
   end record;

   function In_Range (Value : Integer; Bounds : Int_Range) return Boolean;
end Pkg;
