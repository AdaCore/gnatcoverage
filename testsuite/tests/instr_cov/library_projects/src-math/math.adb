package body Math is

   function From_Integer (Value : Integer) return Number is
   begin
      return (Value => Value);
   end From_Integer;

   function "+" (Left, Right : Number) return Number is
   begin
      return (Value => Left.Value + Right.Value);
   end "+";

   function "-" (Left, Right : Number) return Number is
   begin
      return (Value => Left.Value - Right.Value);
   end "-";

end Math;
