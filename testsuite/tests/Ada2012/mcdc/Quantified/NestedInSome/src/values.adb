pragma Ada_2012;

package body Values is
   function Some_Pos_Or_Even (S1: Sequence) return Boolean is
   begin
      return (for some K in S1'Range => S1(K) > 0 or else S1(K) mod 2 = 0); -- # eval
   end Some_Pos_Or_Even;
end Values;
