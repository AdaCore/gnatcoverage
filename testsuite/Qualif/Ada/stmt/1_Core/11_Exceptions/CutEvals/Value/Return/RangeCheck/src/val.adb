with Args; use Args;

package body Val is

   pragma Unsuppress (All_Checks);

   function To_Bool (A : Integer) return Boolean is
   begin
      -- Possible range check failure on conversion here
      return Bool (Boolval(A)); -- # eval
   end;

   procedure Bool (A : Integer; R : out Boolean) is
   begin
      R := To_Bool (A); -- # eval
   end;
   
end;
