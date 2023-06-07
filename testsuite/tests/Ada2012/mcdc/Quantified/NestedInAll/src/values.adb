pragma Ada_2012;

package body Values is
   function All_Pos_And_Even (S1: Sequence) return Boolean is
   begin
      return (for all X of S1 => X > 0 and then X mod 2 = 0); -- # eval
   end All_Pos_And_Even;
end Values;
