procedure Assert (B : Boolean) is
begin
   if not B then
      raise Program_Error;
   end if;
end Assert;
