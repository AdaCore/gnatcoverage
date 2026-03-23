pragma Ada_2022;

package body Andnot is

   function F (A, B : Boolean) return Boolean is
      type Zero_Or_One is range 0 .. 1;  -- # decl
      type Unbouded is array (Zero_Or_One range <>) of Boolean; -- # decl
      subtype Singleton is Unbouded (1 .. 1);  -- # decl
      Sing : constant Singleton := [True];  -- # decl
      Res  : constant Unbouded := [for Elt of Sing when A and then not B => Elt];  -- # evalStmt :o/d:
   begin
      return Res'Length = 1;  -- # returnValue
   end;
end;
