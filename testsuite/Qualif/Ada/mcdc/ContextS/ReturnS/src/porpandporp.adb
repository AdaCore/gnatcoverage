package body PorPandPorP is
   function F (A, B, C, D : Boolean) return Boolean is
   begin
      return (A or else B) and then (C or else D); -- # evaluate
   end;
end;
