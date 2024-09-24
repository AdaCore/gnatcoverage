package body Pkg is

   procedure Swap (X, Y : Bool_Acc) is
      Tmp : Boolean;
   begin
      if X = null then        -- # exempt_1_d
         raise Program_Error; -- # exempt_1_s
      end if;                 -- # exempt_1

      if Y = null then        -- # exempt_2_d
         raise Program_Error; -- # exempt_2_s
      end if;                 -- # exempt_2
      Tmp := X.all;           -- # ok
      X.all := Y.all;         -- # ok
      Y.all := Tmp;           -- # ok
   end Swap;

end Pkg;
