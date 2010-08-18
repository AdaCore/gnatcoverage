with Support;
package body Counters is

   function Zero return Integer is
   begin
      return Support.Identity (0); -- # Zero
   end;

   procedure Inc_X is
   begin
      X := X + 1;               -- # IncX
      declare
         Zi : Integer := Zero;  -- # IncX
      begin
         X := X + Zi;           -- # IncX
      end;
   end;

   procedure Dec_X is
   begin
      X := X - 1;               -- # DecX
      declare
         Zd : Integer := Zero;  -- # DecX
      begin
         X := X + Zd;           -- # DecX
      end;
   end;
end;


