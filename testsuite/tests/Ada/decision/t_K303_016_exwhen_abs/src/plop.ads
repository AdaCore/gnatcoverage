package Plop is
   type Step is new Long_Integer;
   type Value is new Long_Long_Float;
   function Steps_To_LT (X, LB : Value; Max : Step) return Step;
end;
