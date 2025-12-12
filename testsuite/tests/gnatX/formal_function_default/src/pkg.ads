pragma Ada_2022;
pragma Extensions_Allowed (On);

package Pkg is

   type Point is record
      X, Y : Integer;
   end record;

   Width  : constant := 4;
   Height : constant := 3;

   generic
      with function Is_Center (Self : Point) return Boolean is
        (Self.X = 0 and then Self.Y = 0);  -- # default
   package Grid is
      function Point_Image (Self : Point) return Character;
      procedure Print;
   end Grid;

   function Bottom_Right_Is_Center (Self : Point) return Boolean is
     (Self = (Width - 1, Height - 1));  -- # override

end Pkg;
