with Support;

package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
   begin
      return Support.Identity (A or else B);  -- # orelse
   end;
end;



