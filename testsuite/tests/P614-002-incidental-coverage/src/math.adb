package body Math is

   N_Adds, N_Mults : Natural := 0;

   procedure Inc (X : in out Integer) is
   begin
      X := X + 1; -- # inc
   end;

   function Eval (Op : Binop_T; X, Y : Integer) return Integer is
   begin
      if Op = Add then  -- # check-add
         Inc (N_Adds);  -- # add
         return X + Y;  -- # add
      end if;

      if Op = Mult then -- # check-mult
         Inc (N_Mults); -- # mult
         return X * Y;  -- # mult
      end if;
   end;

end;
