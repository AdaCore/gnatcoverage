package Values is
   type Sequence is array (Natural range <>) of Integer;

   function Some_Pos (S1, S2: Sequence) return Boolean;
end Values;
