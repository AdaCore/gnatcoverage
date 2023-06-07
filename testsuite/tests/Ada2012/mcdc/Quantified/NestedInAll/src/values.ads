package Values is
   type Sequence is array (Natural range <>) of Integer;

   function All_Pos_And_Even (S1: Sequence) return Boolean;
end Values;
