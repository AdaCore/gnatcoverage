pragma Ada_2012;
package body Flip is
   function F (X : Boolean) return Boolean is
   begin
      return (if not X then True else False);    -- # ifx-eval :o/d:
   end;
end;

