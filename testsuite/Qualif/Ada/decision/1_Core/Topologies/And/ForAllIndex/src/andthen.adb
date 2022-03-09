pragma Ada_2012;
package body Andthen is

   type Irange is new Integer range -2 .. 4;

   function And_Then (A, B : Boolean) return Boolean is
      AA : array (Irange) of Boolean := (others => A); -- # decl
      BB : array (Irange) of Boolean := (others => B); -- # decl
   begin
      return (for all I in Irange => AA(I) and then BB(I)); -- # andthen :o/d:
   end;
end;
