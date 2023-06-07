pragma Ada_2012;

package Std is
   type Category is (Standard, System, Premium);

   Cat : Category := Premium;
   pragma Volatile (Cat);

   procedure Process (X : in out Integer);
end;
