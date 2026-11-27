package body Pkg is

   -----------------------
   -- Cartesian_Product --
   -----------------------

   function Cartesian_Product (Self : T) return Integer is
   begin
      return Self.X * Self.Y;
   end Cartesian_Product;

end Pkg;
