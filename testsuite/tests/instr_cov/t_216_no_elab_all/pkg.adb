package body Pkg is

   --  Because of the No_Elaboration_Code_All pragma in the spec, gnatcov must
   --  not add witness calls for the elaboration of the following type
   --  declaration.

   type New_Int is new Integer;

   --------------
   -- In_Range --
   --------------

   function In_Range (Value : Integer; Bounds : Int_Range) return Boolean is
      V : constant New_Int := New_Int (Value);
      L : constant New_Int := New_Int (Bounds.Low_Bound);
   begin
      return V >= L and then Value <= Bounds.High_Bound;
   end In_Range;

end Pkg;
