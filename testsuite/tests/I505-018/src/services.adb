package body Services is

   Counter : Integer;

   procedure To_Be_Called is
   begin
      Counter := Counter + 1;
   end;

   procedure Not_To_Be_Called is
   begin
      raise Program_Error;
   end;
end;


