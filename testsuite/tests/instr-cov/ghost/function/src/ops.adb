pragma Ada_2012;

with Common;

package body Ops is
   function Bumpable (X : Integer) return Boolean is
   begin
      -- Decision on purpose here, just to strengthen the test

      if X < Integer'last then -- # ghost-if
         return True;          -- # ghost-then
      else
         return False;         -- # ghost-else
      end if;
   end;

   procedure Bump (X : in out Integer) is
   begin
      X := X + 1; -- # bump
   end;
end;
