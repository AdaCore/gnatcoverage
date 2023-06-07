package body Lib is
   
   --  We will be checking full object coverage by just exercising the source
   --  condition True and False. Possible overflow checks are not part of the
   --  goal.
   
   pragma Suppress (All_Checks);
   
   procedure Adjust (X : in out Integer) is
   begin
      if X > 0 then
         X := X - 1;
      else
         X := X + 1;
      end if;
   end;
   
end;
   
