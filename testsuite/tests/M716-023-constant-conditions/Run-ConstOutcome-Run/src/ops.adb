package body Ops is
   function Eval (Idx : Cond_Index) return Boolean is
   begin
      if                      -- # cond
        (Cond_1 (Idx)         -- # first
            and then          -- # op
         Cond_1'Length /= 0)  -- # second
            or else           -- # op
         Cond_2 (Idx)         -- # third
      then
         return True;         -- # true
      else
         return False;        -- # false
      end if;
   end;
end;
