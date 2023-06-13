procedure Assert (X : Boolean) is
begin
   if not X then
      raise Program_Error;
   end if;
end Assert;
