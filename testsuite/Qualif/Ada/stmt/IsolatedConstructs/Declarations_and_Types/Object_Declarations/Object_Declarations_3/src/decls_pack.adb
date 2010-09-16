package body Decls_Pack is

   procedure Local_Swap (C1, C2 : in out Coordinate) is
      Tmp : Coordinate := C1;                                 -- # local_swap
   begin
      C1 := C2;                                               -- # local_swap
      C2 := Tmp;                                              -- # local_swap
   end Local_Swap;

   function Local_Fun (Arg : Var_String) return Var_String is
      Result : Var_String := Arg;                             -- # decl
   begin
      for I in 1 .. Arg.Len loop                              -- # stmt
         Result.Data (I) := Character'Succ (Result.Data (I)); -- # stmt
      end loop;

      return Result;                                          -- # stmt
   end Local_Fun;

end Decls_Pack;
