pragma Ada_2012;
package body Flip is

   type Irange is new Integer range 0 .. 0;

   function F (X : Boolean) return Boolean is
      AA : array (Irange) of Boolean := (others => X); -- # decl
   begin
      return (for some I in Irange => not AA(I)); -- # ifx-eval :o/d:
   end;
end;
