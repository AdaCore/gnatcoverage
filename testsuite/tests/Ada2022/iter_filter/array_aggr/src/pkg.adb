pragma Ada_2022;

package body Pkg is

   function Non_Zero_Mult (Arr : Arr_T) return Integer is
      Res          : Integer := 1;  -- # other_stmt
      Filtered_Arr : Arr_T :=  -- # other_stmt
        [for Element of Arr  -- # other_cont
         when Element < 0 or else Element > 0  -- # filter
         => Element];  -- # other_cont
   begin
      for Element of Filtered_Arr loop  -- # other_stmt
         Res := Res * Element;  -- # loop_body
      end loop;
      return Res;  -- # other_stmt
   end Non_Zero_Mult;

end Pkg;
