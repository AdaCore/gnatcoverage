package body Pkg is

   --------------------
   -- Reset_Coverage --
   --------------------

   procedure Reset_Coverage is
   begin
      pragma Annotate (Xcov, Reset_Buffers);
   end Reset_Coverage;

   -------------------
   -- Dump_Coverage --
   -------------------

   procedure Dump_Coverage is
   begin
      pragma Annotate (Xcov, Dump_Buffers);
   end Dump_Coverage;

   ----------
   -- Fact --
   ----------

   function Fact (I : Integer) return Integer is
   begin
      if I < 2 then
         return 1;
      else
         return I * Fact (I - 1);
      end if;
   end Fact;

end Pkg;
