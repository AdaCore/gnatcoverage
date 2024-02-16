procedure Main is
   function Inner (X : Boolean) return Boolean is separate;

   X : Boolean := True;
   pragma Volatile (X);
begin
   X := Inner (X);
end Main;
