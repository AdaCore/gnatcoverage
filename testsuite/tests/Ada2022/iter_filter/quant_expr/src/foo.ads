pragma Ada_2022;

package Foo is

   type Arr_T is array (Positive range <>) of Integer;

   function All_Naturals_Are_Non_Zero (Arr : Arr_T) return Boolean is
     (for all Element of Arr  -- # stmt
      when Element >= 0       -- # filter
      => Element /= 0);       -- # pred

end Foo;
