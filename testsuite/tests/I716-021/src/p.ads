with System;

package P is
   subtype Index_T is Integer range 0 .. 255;
   type Block_T is array (Integer range <>) of Integer;

   function Head_Of (B : Block_T) return System.Address;
end;
