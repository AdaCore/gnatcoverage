pragma ada_2012;
package body pandpor is
   subtype Irange is Integer range 1 .. 1;

   function F_1 (X, Y, Z : Boolean) return Boolean is
      A : array (Irange) of Boolean := (others => X); -- # decl
      B : array (Irange) of Boolean := (others => Y); -- # decl
      C : array (Irange) of Boolean := (others => Z); -- # decl
   begin
      return (for some I in Irange => (A(I) and then B(I)) or else C(I)); -- # evalStmt :o/d:
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return F_1 (X => A, Y => B, Z => C); -- # returnValue
   end;
end;
