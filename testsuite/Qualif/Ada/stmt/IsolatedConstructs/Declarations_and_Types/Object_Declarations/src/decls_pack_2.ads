--  This package contains library-level access object declaration with explicit
--  initialization expression and subprograms containing local access object
--  declarations with and without explicit initialization that are expected to
--  be covered only when subprograms are called.
--  Also contains generic packages that do the same for other kinds of types -
--  derived records and private.

with Decls_Support; use Decls_Support;
with Support;       use Support;
package Decls_Pack_2 is

   Access_All_Integer_Var : Access_All_Integer := Integer_Var'Access;-- # dcls

   I : Integer := Identity (1);                                      -- # dcls
   --  Needed to avoid creating a dummy xcov report

   procedure Local_Swap (V1, V2 : in out Access_Integer);

   function Local_Fun (I : Integer) return Access_Const_Integer;
   --  If I > 0 returns access value pointing to the value of I, otherwise
   --  returns null

   generic
   package Decls_Pack_Derived_Records_G is

      Derived_Discrete_Coordinate_V : Derived_Discrete_Coordinate; -- # g1_dcls

      procedure Local_Swap (V1, V2 : in out Derived_Coordinate);

      function Local_Fun (C1, C2 : Float) return Derived_Coordinate;
      --  If C1 > 0.0 and C2 > 0.0, returns coordinates (C1, C2), otherwise
      --  returns coordinates (0.0, 0.0)
   end Decls_Pack_Derived_Records_G;

   generic
   package Decls_Pack_Private_G is

      T_Private_C : constant T_Private := T_Private_Zero;    -- # g2_dcls

      procedure Local_Swap (V1, V2 : in out T_Private);

      function Local_Fun (Arg : T_Private) return T_Private;
      --  Some meaningless computations...
   end Decls_Pack_Private_G;

end Decls_Pack_2;
