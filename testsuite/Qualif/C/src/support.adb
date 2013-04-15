package body Support is

   procedure Assert (Cond : Boolean) is
   begin
      if not Cond then
         raise Program_Error;
      end if;
   end;

end;
