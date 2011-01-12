package body FUAND is

   function Both_In_Range (Ops : Sensor_Pair) return Boolean is
      A : Sensor renames Ops.A; -- prevent multiple Ops on # eval
      B : Sensor renames Ops.B;
   begin
      return Ops.A.V in A.Lo .. A.Hi -- # evalA
        and then Ops.B.V in B.Lo .. B.Hi;  -- # evalB
   end;
end;
