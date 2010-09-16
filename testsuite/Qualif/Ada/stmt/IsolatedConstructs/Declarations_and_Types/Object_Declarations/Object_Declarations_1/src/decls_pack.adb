package body Decls_Pack is

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

end Decls_Pack;
