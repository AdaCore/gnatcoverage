package body Class_Wide_Ops is
   procedure Handle_Alert (A : in out Alert'Class) is
   begin
      Handle (A);
   end Handle_Alert;
end;

