package body AndCor is
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C;  -- # orelse :o/e:
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return A and then Orelse (B, C); -- # andthen :o/e:
   end;
end;
