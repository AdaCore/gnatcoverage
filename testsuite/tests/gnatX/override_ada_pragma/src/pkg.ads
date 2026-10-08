--  The second pragma overrides the first one: this source is interpreted as
--  Ada 2022. Make sure "gnatcov instrument" leverages this to instrument the
--  formal expression function.

pragma Ada_2005;
pragma Extensions_Allowed (On);

package Pkg is

   generic
      with function Predicate (Self : Integer) return Boolean is
        (Self >= 0);  -- # expr
   package Gen is
      procedure Check (Self : Integer);
   end Gen;

end Pkg;
