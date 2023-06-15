package body Pkg is

   ----------
   -- Fact --
   ----------

   function Fact (N : int) return int is
   begin
      if N <= 1 then
         return N;
      else
         return N * Fact (N - 1);
      end if;
   end Fact;

   -----------
   -- Check --
   -----------

   procedure Check (Cond : int) is
   begin
      if Cond = 0 then
         raise Program_Error;
      end if;
   end Check;

end Pkg;
