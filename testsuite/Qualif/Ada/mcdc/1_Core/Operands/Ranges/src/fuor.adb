package body FUOR is

   function One_In_Range (Ops : Sensor_Pair) return Boolean is
      A : Sensor renames Ops.A; -- # decl
      B : Sensor renames Ops.B; -- # decl
      -- prevent multiple Ops on # eval
   begin
      return Ops.A.V in A.Lo .. A.Hi -- # evalA
        or else Ops.B.V in B.Lo .. B.Hi;  -- # evalB
   end;
end;
