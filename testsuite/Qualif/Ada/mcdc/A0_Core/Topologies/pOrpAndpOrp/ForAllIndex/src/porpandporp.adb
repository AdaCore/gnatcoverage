pragma ada_2012;
package body pOrpAndpOrp is
   subtype Irange is Integer range 1 .. 4;

   function F_1 (X, Y, Z, T : Boolean) return Boolean is
      A : array (Irange) of Boolean := (others => X); -- # decl
      B : array (Irange) of Boolean := (others => Y); -- # decl
      C : array (Irange) of Boolean := (others => Z); -- # decl
      D : array (Irange) of Boolean := (others => T); -- # decl
   begin
      return (for all I in Irange => ((A(I) or else B(I)) and then (C(I) or else D(I)))); -- # evalStmt :o/d:
   end;

   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return F_1 (X => A, Y => B, Z => C, T => D); -- # returnValue
   end;
end;
