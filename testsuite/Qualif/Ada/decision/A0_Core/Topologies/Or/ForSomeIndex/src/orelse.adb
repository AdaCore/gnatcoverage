pragma Ada_2012;
package body Orelse is

   type Irange is new Integer range 1 .. 3;

   function Or_Else (A, B : Boolean) return Boolean is
      AA : array (Irange) of Boolean := (others => A); -- # decl
      BB : array (Irange) of Boolean := (others => B); -- # decl
   begin
      return (for some I in Irange => AA(I) or else BB(I)); -- # orelse :o/d:
   end;
end;
