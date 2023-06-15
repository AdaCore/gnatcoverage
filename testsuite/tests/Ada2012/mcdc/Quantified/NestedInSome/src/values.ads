package Values is
   type Sequence is array (Natural range <>) of Integer;
   function Some_Pos_Or_Even (S1: Sequence) return Boolean;
end Values;
