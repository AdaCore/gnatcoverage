package body Decls_Pack is

   procedure Local_Swap (V1, V2 : in out Derived_Coordinate) is
      Tmp : Derived_Coordinate;                         -- # local_swap
   begin
      if V1 /= V2 then                                  -- # local_swap
         Tmp := V1;                                     -- # if_local_swap
         V1  := V2;                                     -- # if_local_swap
         V2  := Tmp;                                    -- # if_local_swap
      end if;
   end Local_Swap;

   function Local_Fun (C1, C2 : Float) return Derived_Coordinate is
      Result : Derived_Coordinate;                      -- # decl
   begin

      if C1 > 0.0 and then C2 > 0.0 then                -- # stmt
        Result.X := C1;                                 -- # in_if
        Result.Y := C2;
      end if;

      return Result;                                    -- # stmt
   end Local_Fun;

end Decls_Pack;
