package body Renamed_Objects is

   function Get_Index (I : Integer) return Integer is
   begin
      if I in 1 .. 10 then                    -- # get_index
         return I;                            -- # if_get_index
      else
         return 10;                           -- # else_get_index
      end if;
   end Get_Index;

end Renamed_Objects;
