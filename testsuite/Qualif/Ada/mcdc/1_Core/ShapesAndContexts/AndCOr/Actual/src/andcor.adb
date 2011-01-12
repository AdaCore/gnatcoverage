with Support;

package body AndCOr is

   function One_Of (A, B : Boolean) return Boolean is
   begin
      return A;  -- # returnValue
   end;
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return Support.Identity (B or else C); -- # orelse
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      return One_Of (A and then Orelse (B, C),   -- # andthen
                     A and then Orelse (B, C));  -- # andthen
   end;

end;
