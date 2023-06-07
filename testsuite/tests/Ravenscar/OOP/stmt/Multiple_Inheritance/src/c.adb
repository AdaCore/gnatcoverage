package body C is
   procedure Class_Wide_IC (X : in out IC'Class) is
   begin
      if Test_IC (X) then     -- # class_wide_ic
         Class_Wide_IA (X);   -- # if_cw_ic
      else
         Class_Wide_IB (X);   -- # else_cw_ic
      end if;
   end Class_Wide_IC;
end C;
