pragma Ada_2012;

package body Values is
   function All_Pos_Some_neg (S1, S2: Sequence) return Boolean is
   begin
      return (for all K in S1'Range => S1(K) > 0) and then (for some E of S2 => E < 0); -- # eval
   end;
end Values;
