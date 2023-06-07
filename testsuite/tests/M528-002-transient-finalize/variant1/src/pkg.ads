with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pkg is
   
   abc  : constant Unbounded_String := To_Unbounded_String ("abc");
   def  : constant Unbounded_String := To_Unbounded_String ("def");
   ghi  : constant Unbounded_String := To_Unbounded_String ("ghi");
   bool : constant Unbounded_String := To_Unbounded_String ("bool");

   function Foo (Var : Unbounded_String) return Boolean;
   
end;
