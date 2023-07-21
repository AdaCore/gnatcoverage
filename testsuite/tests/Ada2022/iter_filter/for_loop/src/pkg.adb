pragma Ada_2022;

package body Pkg is

   function Non_Zero_Mult (Arr : Arr_T) return Integer is
      Res : Integer := 1;  -- # other_stmt
   begin
      for Element of Arr  -- # other_stmt
         when Element < 0 or else Element > 0 -- # filter
      loop
         Res := Res * Element;  -- # loop_body
      end loop;
      return Res;  -- # other_stmt
   end Non_Zero_Mult;

end Pkg;
