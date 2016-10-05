package body Objects is
   
   procedure Register_Pos is
   begin
      N_Positives := N_Positives + 1; -- # pos
   end;
   
   procedure Proxy_Characterize (X : Integer) is
      
      procedure Characterize (X : Integer);
      pragma No_Return (Characterize); -- # pragma
      
      procedure Characterize (X : Integer) is
      begin
         if X > 0 then     -- # test
            Register_Pos;  -- # pos
         end if;
         raise Constraint_Error; -- # test
      end;
   
   begin
      Characterize (X); -- # test
   end;
end;
