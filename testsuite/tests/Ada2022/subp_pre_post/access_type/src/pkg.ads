pragma Assertion_Policy (Check);
pragma Ada_2022;

package Pkg is

   type Point is record
      X, Y : Integer;
   end record;

   subtype Screen_X is Integer range 1 .. 256;
   subtype Screen_Y is Integer range 1 .. 192;

   type Point_Adjuster is
     access procedure (Self : in out Point)
     with
       Pre  => Self.X in Screen_X and then Self.Y in Screen_Y, -- # acc-pre
       Post => Self.X in Screen_X and then Self.Y in Screen_Y; -- # acc-post

   procedure Adjust (Self : in out Point; Adjuster : Point_Adjuster);

   procedure Div_2 (Self : in out Point);

   procedure Test (Self : in out Point);

end Pkg;
