pragma Ada_2022;

with Ada.Containers.Ordered_Sets;

package body Pkg is

   package Int_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);
   subtype Set is Int_Sets.Set;

   function Non_Zero_Mult (Arr : Arr_T) return Integer is
      Res          : Integer := 1;  -- # other_stmt
      Filtered_Set : Set :=  -- # other_stmt
        [for Element of Arr  -- # other_cont
         when Element < 0 or else Element > 0  -- # filter
         => Element];  -- # other_cont
   begin
      for Element of Filtered_Set loop  -- # other_stmt
         Res := Res * Element;  -- # loop_body
      end loop;
      return Res;  -- # other_stmt
   end Non_Zero_Mult;

end Pkg;
