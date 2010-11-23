package body BB_Null is
   procedure Do_Nothing is
   begin
      null;  -- # nullInProc
   end;
begin
   null;     -- # nullInElab
end;
