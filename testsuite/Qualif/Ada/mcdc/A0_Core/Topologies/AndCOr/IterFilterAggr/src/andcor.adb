pragma Ada_2022;

with Support; use Support;

package body AndCOr is

   type Zero_Or_One is range 0 .. 1;  -- # decl
   type Unbouded is array (Zero_Or_One range <>) of Boolean; -- # decl
   subtype Singleton is Unbouded (1 .. 1);  -- # decl
   Sing : constant Singleton := [True];  -- # decl

   function Orelse (B, C : Boolean) return Boolean is
      Res  : constant Unbouded := [for Elt of Sing when B or else C => Elt];  -- # orelse :o/d:
   begin
      return Res'Length = 1; -- # returnOr
   end Orelse;

   function F (A, B, C : Boolean) return Boolean is
      Res  : constant Unbouded := [for Elt of Sing when A and then Orelse (B, C) => Elt];  -- # andthen :o/d:
   begin
      return Res'Length = 1;  -- # returnValue
   end F;
end AndCOr;
