pragma Ada_2012;

package Pkg is

   type Point is record                                       -- # decl
      X, Y : Float;                                           -- # decl
   end record;                                                -- # decl

   function Set (X, Y : Float) return Point is (X, Y);        -- # set

   function Reset (X, Y : Float) return Point is (0.0, 0.0);  -- # reset

end Pkg;
