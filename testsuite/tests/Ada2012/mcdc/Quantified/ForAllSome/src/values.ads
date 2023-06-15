package Values is
   type Sequence is array (Natural range <>) of Integer;

   function All_Pos_Some_Neg (S1, S2: Sequence) return Boolean;
end Values;
