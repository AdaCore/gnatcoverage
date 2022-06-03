pragma Ada_2012;
package body Value is

   function F (X : Boolean) return Boolean is
      AA : array (1 .. 2) of Boolean := (X, X); -- # decl
   begin
      return (for some B of AA => B);       -- # ifx-eval
   end;
end;
