pragma Ada_2022;
pragma Assertion_Policy (Check);

package Pkg is

   type Point is private
   with Default_Initial_Condition =>
     Get_X (Point) >= 0 or else Get_Y (Point) >= 0; -- # dic

   procedure Set_Defaults (X, Y : Integer);

   function Create (X, Y : Integer) return Point;

   function Get_X (Self : Point) return Integer;
   function Get_Y (Self : Point) return Integer;

   procedure Print (Self : Point);

private

   Default_X : Integer := 0;
   Default_Y : Integer := 0;

   type Point is record
      X : Integer := Default_X;
      Y : Integer := Default_Y;
   end record;

end Pkg;
