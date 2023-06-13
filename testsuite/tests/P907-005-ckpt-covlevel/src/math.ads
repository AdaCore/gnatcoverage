package Math is
   type Binop_T is (Add, Mult);
   function Eval (Op : Binop_T; X, Y : Integer) return Integer;
end;
