pragma Ada_2012;
package body Expr is
   type Irange is new Integer range -2 .. 4;

   function F (A, B, C, D : Boolean) return Boolean is
      AA : array (Irange) of Boolean := (others => A); -- # decl
      BB : array (Irange) of Boolean := (others => B); -- # decl
      CC : array (Irange) of Boolean := (others => C); -- # decl
      DD : array (Irange) of Boolean := (others => D); -- # decl
   begin
      return (for all I in Irange => (AA(I) and then BB(I)) or else (CC(I) and then DD(I))); -- # eval :o/d:
   end;
end;



