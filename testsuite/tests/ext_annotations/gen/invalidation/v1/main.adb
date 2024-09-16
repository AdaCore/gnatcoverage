procedure Main is
   function Nested (X : Integer) return Integer is
   begin
      return X;
   end Nested;

   Y : Integer;
   pragma Volatile (Y);
begin
   Y := Nested (2);
end Main;
