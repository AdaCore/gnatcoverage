with Values; use Values;

procedure Test_LT0 is
   procedure Assert (T : Boolean) is
   begin
      if not T then
         raise Program_Error;
      end if;
   end;
begin
   Scan (-5);
   Assert (N_LT0 = 1);
   Assert (N_GE0 = 0);
end;

