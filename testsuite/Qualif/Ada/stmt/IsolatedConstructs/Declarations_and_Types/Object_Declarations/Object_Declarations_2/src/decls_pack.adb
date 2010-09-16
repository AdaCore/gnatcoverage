package body Decls_Pack is

   procedure Local_Swap (M1, M2 : in out Matrix) is
      Tmp : Matrix := M1;                   -- # local_swap
   begin
      M1 := M2;                             -- # local_swap
      M2 := Tmp;                            -- # local_swap
   end Local_Swap;

   function Local_Fun (Arg : Matrix) return Matrix is
      Result : Matrix := Arg;                -- # decl
   begin
      for I in Result'Range (1) loop         -- # stmt
         for J in Result'Range (2) loop      -- # stmt
            Result (I, J) := -Result (I, J); -- # stmt
         end loop;
      end loop;

      return Result;                         -- # stmt
   end Local_Fun;

end Decls_Pack;
