package body Decls_Pack is

   procedure Local_Swap (V1, V2 : in out T_Private) is
      Tmp : T_Private := V1;                                 -- # local_swap
   begin
      V1 := V2;                                              -- # local_swap
      V2 := Tmp;                                             -- # local_swap
   end Local_Swap;

   function Local_Fun (Arg : T_Private) return T_Private is
      Result : T_Private;                                    -- # decl
   begin

      case Get_Integer (Arg) is                           -- # stmt
         when 1 =>
            Result := Get_Private (100);                     -- # case1
         when 2 =>
            Result := T_Private_Zero;                        -- # case2
         when others =>
            null;                                            -- # case3
      end case;

      return Result;                                          -- # stmt
   end Local_Fun;

end Decls_Pack;
