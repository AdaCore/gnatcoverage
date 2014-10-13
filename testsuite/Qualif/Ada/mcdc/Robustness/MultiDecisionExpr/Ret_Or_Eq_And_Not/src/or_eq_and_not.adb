function Or_Eq_And_Not (A, B, C, D : Boolean) return Boolean is
begin
   return (A or else B) = (C and then not D); -- # eval
end;
