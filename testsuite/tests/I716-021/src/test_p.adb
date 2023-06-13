with System, P; use P;

procedure Test_P is
   My_Block : Block_T (1 ..2) := (1, 2);
   My_Empty_Block : Block_T (2 .. 1);

   Addr : System.Address;
   use type System.Address;
begin
   Addr := Head_Of (My_Block);
   if Addr /= My_Block (My_Block'First)'Address then
      raise Program_Error;
   end if;

   Addr := Head_Of (My_Empty_Block);
   if Addr /= System.Null_Address then
      raise Program_Error;
   end if;
end;
