pragma ada_2012;
package body Andnot is
   subtype Irange is Integer range 1 .. 4;

   function F_1 (X, Y : Boolean) return Boolean is
      A : array (Irange) of Boolean := (others => X); -- # decl
      B : array (Irange) of Boolean := (others => Y); -- # decl
   begin
      return (for all I in Irange => A(I) and then not B(I)); -- # evalStmt :o/d:
   end;

   function F (A, B : Boolean) return Boolean is
   begin
      return F_1 (X => A, Y => B); -- # returnValue
   end;
end;
