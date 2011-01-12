package body FUOR is

   function Match_P_Or_S (Op : Block; P, S : Piece) return Boolean is
   begin
      return Op (P'Range) = P  -- # evalA
        or else Op (Block'Last - S'Length + 1 .. Block'last) = S; -- # evalB
   end;
end;
