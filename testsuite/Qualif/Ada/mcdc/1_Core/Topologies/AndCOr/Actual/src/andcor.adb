with Support; use Support;

package body AndCOr is

   function Orelse (B, C : Boolean) return Boolean is
   begin
      return Value (B or else C); -- # orelse
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      return Value (A and then Orelse (B, C));   -- # andthen
   end;

end;
