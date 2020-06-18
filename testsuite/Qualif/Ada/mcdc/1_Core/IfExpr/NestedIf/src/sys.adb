pragma Ada_2012;
with Ops; use Ops;

package body Sys is

   function Andthen (A, B : Boolean) return Boolean is
   begin
      return (if A then (if B then True else False) else False); -- # eval
   end;
end;
