pragma Ada_2012;

package body Values is
   function Some_Pos (S1, S2: Sequence) return Boolean is
   begin
      return (for some K in S1'Range => S1(K) > 0) or else (for some E of S2 => E > 0); -- # eval
   end;
end Values;
