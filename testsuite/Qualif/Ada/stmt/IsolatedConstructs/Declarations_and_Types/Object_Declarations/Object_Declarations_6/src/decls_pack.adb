package body Decls_Pack is

   procedure Local_Swap (V1, V2 : in out Access_Integer) is
      Tmp : Access_Integer := V1;  -- # local_swap
   begin
      V1  := V2;                   -- # local_swap
      V2  := Tmp;                  -- # local_swap
   end Local_Swap;

   function Local_Fun (I : Integer) return Access_Const_Integer is
      Result : Access_Const_Integer := new Integer'(I);  -- # decl
   begin

      if I < 0 then                                      -- # stmt
        Result := null;                                  -- # in_if
      end if;

      return Result;                                     -- # stmt
   end Local_Fun;

end Decls_Pack;
