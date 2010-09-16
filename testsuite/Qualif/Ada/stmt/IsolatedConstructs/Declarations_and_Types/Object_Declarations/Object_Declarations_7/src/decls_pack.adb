package body Decls_Pack is

   procedure Local_Swap (V1, V2 : in out Access_Integer) is
      Tmp : Access_Integer;  -- # local_swap
   begin
      if V1 /= V2 then       -- # local_swap
         Tmp := V1;          -- # if_local_swap
         V1  := V2;          -- # if_local_swap
         V2  := Tmp;         -- # if_local_swap
      end if;
   end Local_Swap;

   function Local_Fun (I : Integer) return Access_Const_Integer is
      Result : Access_Const_Integer;  -- # decl
   begin

      if I > 0 then                   -- # stmt
        Result := new Integer'(I);    -- # in_if
      end if;

      return Result;                  -- # stmt
   end Local_Fun;

end Decls_Pack;
