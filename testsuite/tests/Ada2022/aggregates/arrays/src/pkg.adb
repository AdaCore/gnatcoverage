pragma Ada_2022;

package body Pkg is

   function Absolute (Input : Int_Arr) return Int_Arr is
   begin
      if Input = [] then  -- # empty_aggr_guard
         return [];  -- # empty_aggr_st
      elsif Input'Length = 1 then  -- # single_elt_guard
         return                                                 -- # single_elt_st
           [(declare Elt : Integer renames Input (Input'First); -- # single_elt_st
             begin (if Elt > 0 then Elt else -Elt))];           -- # single_elt_dc
      else
         return [for Idx in Input'Range =>  -- # multi_elt_st
                   (declare Elt : Integer renames Input (Idx) ; -- # multi_elt_st
                    begin (if Elt > 0 then Elt else -Elt))];   -- # multi_elt_dc
      end if;
   end Absolute;

end Pkg;
