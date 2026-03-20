package body Decls_Pack_1 is

   procedure Local_Swap (I, J : in out Integer) is
      Tmp : Integer := I;                  -- # local_swap
   begin
      I := J;                              -- # local_swap
      J := Tmp;                            -- # local_swap
   end Local_Swap;

   function Local_Fun (Arg : Week_Day) return Week_Day is
      Day : Week_Day := Arg;               -- # decl
   begin
      case Arg is                          -- # stmt
         when Mon .. Sat =>
            Day := Week_Day'Succ (Day);    -- # case1
         when Sun =>
            Day := Mon;                    -- # case2
      end case;

      return Day;                          -- # stmt
   end Local_Fun;

   package body Decls_Pack_Matrix_G is

      procedure Local_Swap (M1, M2 : in out Matrix) is
         Tmp : Matrix := M1;                   -- # g1_local_swap
      begin
         M1 := M2;                             -- # g1_local_swap
         M2 := Tmp;                            -- # g1_local_swap
      end Local_Swap;

   end Decls_Pack_Matrix_G;

   package body Decls_Pack_Records_G is

      procedure Local_Swap (C1, C2 : in out Coordinate) is
         Tmp : Coordinate := C1;                                 -- # g2_local_swap
      begin
         C1 := C2;                                               -- # g2_local_swap
         C2 := Tmp;                                              -- # g2_local_swap
      end Local_Swap;

   end Decls_Pack_Records_G;

end Decls_Pack_1;
