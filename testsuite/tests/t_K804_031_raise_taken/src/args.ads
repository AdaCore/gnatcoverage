package Args is

   -- Constrained value subject to easy out-of-range check failure

   type Num is range -10 .. 10;

   function Id (X : Num) return Num;
end;
