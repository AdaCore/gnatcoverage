pragma Ada_2012;
package body Value is
   function F (X : Boolean) return Boolean is
   begin
      return (if X then True else False);       -- # ifx-eval
   end;
end;
