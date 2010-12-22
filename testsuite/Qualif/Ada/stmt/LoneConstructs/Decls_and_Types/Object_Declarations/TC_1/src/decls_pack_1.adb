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

      function Local_Fun (Arg : Matrix) return Matrix is
         Result : Matrix := Arg;                -- # g1_decl
      begin
         for I in Result'Range (1) loop         -- # g1_stmt
            for J in Result'Range (2) loop      -- # g1_stmt
               Result (I, J) := -Result (I, J); -- # g1_stmt
            end loop;
         end loop;

         return Result;                         -- # g1_stmt
      end Local_Fun;

   end Decls_Pack_Matrix_G;

   package body Decls_Pack_Records_G is

      procedure Local_Swap (C1, C2 : in out Coordinate) is
         Tmp : Coordinate := C1;                                 -- # g2_local_swap
      begin
         C1 := C2;                                               -- # g2_local_swap
         C2 := Tmp;                                              -- # g2_local_swap
      end Local_Swap;

      function Local_Fun (Arg : Var_String) return Var_String is
         Result : Var_String := Arg;                             -- # g2_decl
      begin
         for I in 1 .. Arg.Len loop                              -- # g2_stmt
            Result.Data (I) := Character'Succ (Result.Data (I)); -- # g2_stmt
         end loop;

         return Result;                                          -- # g2_stmt
      end Local_Fun;

   end Decls_Pack_Records_G;

end Decls_Pack_1;
