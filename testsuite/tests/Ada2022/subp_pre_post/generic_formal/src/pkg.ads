pragma Ada_2022;
pragma Assertion_Policy (Check);

package Pkg is

   type Point is record
      X, Y : Integer;
   end record;

   subtype Screen_X is Integer range 1 .. 256;
   subtype Screen_Y is Integer range 1 .. 192;

   generic
      with procedure Adjuster (Self : in out Point)
        with
          Pre  => Self.X in Screen_X and then Self.Y in Screen_Y, -- # gen-pre
          Post => Self.X in Screen_X and then Self.Y in Screen_Y; -- # gen-post
   procedure Adjust (Self : in out Point);

   generic
      with procedure Adjuster (Self : in out Point)
        with
          Pre  => Self.X in Screen_X and then Self.Y in Screen_Y, -- # gen-pre
          Post => Self.X in Screen_X and then Self.Y in Screen_Y; -- # gen-post
   package Adjust_Pkg is
      procedure Adjust (Self : in out Point);
   end Adjust_Pkg;

   procedure Div_2 (Self : in out Point);

   procedure Test (Self : in out Point);

end Pkg;
