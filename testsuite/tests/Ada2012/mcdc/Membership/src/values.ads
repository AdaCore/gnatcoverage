pragma Ada_2012;

package Values is
   
   type My_Int is new Integer range 1 .. 8;
   
   function Even_Odd (X, Y : My_Int) return Boolean is
      (X in 2 | 4 | 6 | 8 and then Y in 1 | 3 | 5 | 7); -- # eval
end;
