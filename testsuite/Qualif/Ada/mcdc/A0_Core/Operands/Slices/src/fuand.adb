package body FUAND is

   function Match_P_And_S (Op : Block; P, S : Piece) return Boolean is
   begin
      return Op (P'Range) = P  -- # evalA
        and then Op (Block'Last - S'Length + 1 .. Block'last) = S; -- # evalB
   end;
end;
