pragma Ada_2022;

package body AndCOr is

   Items : constant array (Positive range 1 .. 1) of Boolean := [True]; -- # decl

   function Orelse (B, C : Boolean) return Boolean is
      Res   : Boolean := False;  -- # returnOr
   begin
      for Elt of Items when B or else C loop  -- # orelse :o/d:
         Res := Elt;  -- # orTrue
      end loop;
      return Res;  -- # returnOr
   end Orelse;

   function F (A, B, C : Boolean) return Boolean is
      Res   : Boolean := False;  -- # decl
   begin
      for Elt of Items when A and then Orelse (B, C) loop  -- # andthen :o/d:
         Res := Elt;  -- # returnTrue
      end loop;
      return Res;  -- # returnValue
   end F;
end AndCOr;
