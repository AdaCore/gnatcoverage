package body Pkg is

   ----------
   -- Half --
   ----------

   procedure Half (X : Integer) is
   begin
      if X = 0 then
         null;
      end if;

      pragma Annotate (Xcov, Cov_Off, "not of interest");
      if X = 1 then
         null;
      end if;
      pragma Annotate (Xcov, Cov_On);
   end Half;

end Pkg;
