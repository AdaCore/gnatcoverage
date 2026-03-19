package body Ops is
   function Eval (Idx : Cond_Index) return Boolean is
   begin
      if                                     -- # cond
         Cond_Array_Current.Cond (Idx)       -- # first
            and then                         -- # op
         Cond_Array_Current.Cond'Length /= 0 -- # second
            and then                         -- # op
         Cond_2 (Idx)                        -- # third
      then
         return True;                        -- # true
      else
         return False;                       -- # false
      end if;
   end;
end;
