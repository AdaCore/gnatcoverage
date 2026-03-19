with Support; use Support;
package body Pack_1 is

   procedure Proc_1 (I : in out Integer) is
      package Inner_1 is
         J : Integer := I + 1;        -- # proc_1
      end Inner_1;
   begin
      I := I + Inner_1.J;             -- # proc_1
   end Proc_1;

   function Fun (I : Integer) return Integer is
      package Inner is
         J : Integer := 1;            -- # fun
      end Inner;

      package body Inner is
      begin
         J := Identity (1);           -- # fun
      end Inner;

   begin
      return I + Inner.J;             -- # fun
   end Fun;

end Pack_1;
