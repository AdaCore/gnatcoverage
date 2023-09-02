pragma Ada_2022;

with Ada.Containers; use Ada.Containers;

package body Pkg is

   function Overly_Complex_Identity_Build (Input : Int_Set) return Int_Map is
      use Int_Sets;
      use Int_Maps;
   begin
      if Input = [] then  -- # empty_aggr_guard
         return [];  -- # empty_aggr_st
      elsif Input.Length = 1 then  -- # single_elt_guard
         return                                                -- # single_elt_st
           (declare Elt : Integer renames Input.First_Element; -- # single_elt_st
            begin [(If Elt > 0 then Elt else Elt)]);           -- # single_elt_dc
      else
         return [for Elt of Input use                  -- # multi_elt_st
                   (if Elt > 0 then Elt else Elt) =>   -- # multi_elt_dc
                   (if Elt > 0 then Elt else Elt)];    -- # multi_elt_dc
      end if;
   end Overly_Complex_Identity_Build;

end Pkg;
