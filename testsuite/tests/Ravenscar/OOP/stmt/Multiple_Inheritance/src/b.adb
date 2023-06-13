package body B is
   procedure Class_Wide_IB (X : in out IB'Class) is
   begin
      if Test_IB (X) then     -- # class_wide_ib
         P_IB_1 (X);          -- # if_cw_ib
      else
         P_IB_2 (X);          -- # else_cw_ib
      end if;
   end Class_Wide_IB;
end B;
