pragma ada_2012;
package body AndCor is
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return (if B or else C then True else False);  -- # orelse :o/d:
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return (if A and then Orelse (B, C) then True else False); -- # andthen :o/d:
   end;
end;
