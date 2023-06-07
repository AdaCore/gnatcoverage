package body P1 is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C; -- # or
   end;
   
   procedure Combine (A, B, C, D : Boolean) is
   begin
      R := A and then Orelse (B, C) and then D; -- # comb
   end;
   
end;
