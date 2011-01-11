package body FUAND is

   function Match (Op : Block; P : Prefix; S : Suffix) return Boolean is
   begin
      return Op (P'Range) = P  -- # evalA
        and then Op (Block'Last - S'Length + 1 .. Block'last) = S; -- # evalB
   end;
end;
