procedure Main is

   type Int_Range is record
      Low_Bound, High_Bound : Integer;
   end record;

   function In_Range (Value : Integer; Bounds : Int_Range) return Boolean is
   begin
      return Value >= Bounds.Low_Bound and then Value <= Bounds.High_Bound;
   end In_Range;

   Two_Digits : constant Int_Range := (10, 99);
begin
   if In_Range (9, Two_Digits) then
      raise Program_Error;
   elsif not In_Range (10, Two_Digits) then
      raise Program_Error;
   end if;
end Main;
