package body Pkg is

   --------------
   -- In_Range --
   --------------

   function In_Range (Value : Integer; Bounds : Int_Range) return Boolean is
   begin
      return Value >= Bounds.Low_Bound and then Value <= Bounds.High_Bound;
   end In_Range;

end Pkg;
