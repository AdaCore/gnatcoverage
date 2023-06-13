package body A is
   procedure Class_Wide_IA (X : in out IA'Class) is
   begin
      if Test_IA (X) then     -- # class_wide_ia
         P_IA_1 (X);          -- # if_cw_ia
      else
         P_IA_2 (X);          -- # else_cw_ia
      end if;
   end Class_Wide_IA;
end A;
