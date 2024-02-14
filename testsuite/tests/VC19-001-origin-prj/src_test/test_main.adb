with Code;

procedure Test_Main is
   procedure Assert (X : Boolean) is
   begin
      if not X then
         raise Program_Error;
      end if;
   end Assert;
begin
   Assert (Code.Compute (True, False) = Code.Compute (False, True));
end Test_Main;