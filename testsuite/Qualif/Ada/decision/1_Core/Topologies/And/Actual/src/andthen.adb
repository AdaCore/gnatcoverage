with Support;

package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return Support.Identity (A and then B);  -- # andthen :o/0:
   end;
end;



