package Values is
   type Sequence is array (Natural range <>) of Integer;

   function All_Pos (S1, S2: Sequence) return Boolean;

   Value_Seq : Sequence := (1, 2, 3, 4);
end Values;
