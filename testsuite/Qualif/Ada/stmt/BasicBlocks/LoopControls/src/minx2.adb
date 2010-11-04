with Support; use Support;

function MinX2 (X : Natural) return Natural is
   C : Integer := 0;  -- # common
   --  Candidate min value
begin

   --  Loop over the admissible min values and exit as soon
   --  as the actual min value is determined.

   loop
      --  If X is the current admissible min value, go
      --  return that.

      exit when X = C;  -- # common

      --  Otherwise, shift to next candidate and exit if
      --  we're beyond the admissible limit

      C := C + 1;       -- # xgt0
      exit when C > 2;  -- # xgt0
   end loop;

   if C > 2 then -- # common
      return 2;  -- # xgt2
   else
      return X;  -- # xle2
   end if;
end;
