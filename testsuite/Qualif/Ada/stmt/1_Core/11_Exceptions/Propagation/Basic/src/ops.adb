package body Ops is
   
   procedure Div_CE (X, Y : Integer; T : out Integer) is
   begin
      if Y = 0 then -- # stmt
         raise Constraint_Error; -- # fault
      end if;
      T := X / Y; -- # no_fault
   end;
   
   Div_Error : exception;
   
   procedure Div_DE (X, Y : Integer; T : out Integer) is
   begin
      if Y = 0 then -- # stmt
         raise Div_Error; -- # fault
      end if;
      T := X / Y; -- # no_fault
   end;
   
   --
   
   procedure Div
     (X, Y : Integer; T : out Integer; Fault : out Boolean) is
   begin
      begin
         Div_CE (X, Y, T); -- # stmt
         Fault := False; -- # no_fault
      exception
         when Constraint_Error =>
            Fault := True;  -- # fault
         when others =>
            null;  -- # bad_handler
      end;
      
      begin
         Div_DE (X, Y, T); -- # stmt
         Fault := False; -- # no_fault
      exception
         when Div_Error =>
            Fault := True;  -- # fault
         when others =>
            null;  -- # bad_handler
      end;
   end;
   
   
end;
