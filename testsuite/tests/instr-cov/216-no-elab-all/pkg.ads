package Pkg is

   pragma No_Elaboration_Code_All;

   type Int_Range is record
      Low_Bound, High_Bound : Integer;
   end record;

   function In_Range (Value : Integer; Bounds : Int_Range) return Boolean;

end Pkg;
