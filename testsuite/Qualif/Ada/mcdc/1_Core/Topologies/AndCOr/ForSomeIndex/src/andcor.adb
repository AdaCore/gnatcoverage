pragma ada_2012;
package body Andcor is
   subtype Irange is Integer range 1 .. 4;

   function Orelse (Y, Z : Boolean) return Boolean is
      B : array (Irange) of Boolean := (others => Y); -- # returnOr
      C : array (Irange) of Boolean := (others => Z); -- # returnOr
   begin
      return (for some I in Irange => (B(I) or else C(I))); -- # orelse :o/d:
   end;

   function F_1 (X, Y, Z : Boolean) return Boolean is
      A : array (Irange) of Boolean := (others => X); -- # decl
   begin
      return (for some I in Irange => A(I) and then Orelse(Y, Z)); -- # andthen :o/d:
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return F_1 (X => A, Y => B, Z => C); -- # returnValue
   end;
end;
