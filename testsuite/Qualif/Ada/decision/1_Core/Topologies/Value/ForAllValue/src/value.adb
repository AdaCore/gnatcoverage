pragma Ada_2012;
package body Value is

   function F (X : Boolean) return Boolean is
      AA : array (1 .. 2) of Boolean := (others => X); -- # decl
   begin
      return (for all B of AA => B); -- # ifx-eval
   end;
end;
