with Support;

package body CSE is
   procedure Bump (X : in out Integer) is
   begin
      X := X + 1;                             -- # dobump
      for I in 1 .. Support.Identity(0) loop  -- # loop
         X := X + 1;                          -- # noreach
      end loop;
   end;
end;

