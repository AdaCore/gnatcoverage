pragma Ada_2012;

with Common;

package body Ops is

   procedure Bump (X : in out Integer) is
   begin
      X := X + 1; -- # bump
   end;

end;
