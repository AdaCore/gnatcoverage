package body Ops is
   function Eval_Cond
     (Idx : Cond_Index;
      Conds : Cond_Array_Option) return Boolean
   is
   begin
      return                                    -- # stmt-2
         Conds.Available                        -- # first-2
            and then                            -- # op-2
         Conds.Cond (Idx)                       -- # second-2
            and then                            -- # op-2
         Cond_1 (Idx);                          -- # third-2
   end Eval_Cond;

   function Eval (Idx : Cond_Index) return Boolean is
   begin
      return                                    -- # stmt-1
         Cond_2 (Idx)                           -- # first-1
            and then                            -- # op-1
         Eval_Cond (Idx, (True, Cond_1));       -- # second-1
   end Eval;
end Ops;
