--  This package contains library-level private variable object declaration
--  with explicit initialization expression and subprograms containing local
--  private variable object declarations with and without explicit
--  initialization that are expected to be covered only when subprograms are
--  called.

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack is

   Discrete_Coordinate_V : Discrete_Coordinate;       -- # dcls

   procedure Local_Swap (V1, V2 : in out Coordinate);

   function Local_Fun (C1, C2 : Integer) return Discrete_Coordinate;
   --  If C1 > 0 and C2 > 0, returns coordinates (C1, C2), otherwise returns
   --  coordinates (0, 0)
end Decls_Pack;
