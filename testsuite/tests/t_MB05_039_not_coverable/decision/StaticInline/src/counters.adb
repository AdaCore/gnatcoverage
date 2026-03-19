package body Counters is
   procedure Bump (X : in out Natural) is
   begin
      X := X + 1;
   end;
end;
