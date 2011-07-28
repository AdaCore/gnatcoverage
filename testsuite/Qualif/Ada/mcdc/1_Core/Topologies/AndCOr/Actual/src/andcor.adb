with Support; use Support;

package body AndCOr is

   function Orelse (B, C : Boolean) return Boolean is
   begin
      return Value (B or else C); -- # orelse :o/e:
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      return Value (A and then Orelse (B, C));   -- # andthen :o/e:
   end;

end;
