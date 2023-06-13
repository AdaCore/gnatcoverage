package body Ops is
   function Eval (Idx : Cond_Index) return Boolean is
   begin
      return                  -- # stmt
         Cond_1'Length /= 0   -- # first
            and then          -- # op
         Cond_1 (Idx);        -- # second
   end Eval;
end;
