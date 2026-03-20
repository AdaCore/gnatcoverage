package Math is

   type Number is private;

   function From_Integer (Value : Integer) return Number;
   function "+" (Left, Right : Number) return Number;
   function "-" (Left, Right : Number) return Number;

private

   type Number is record
      Value : Integer;
   end record;

end Math;
