package body P is
   function Head_Of (B : Block_T) return System.Address is
      Blength : constant Integer := B'Last - B'First + 1;
      Baddr : System.Address := System.Null_Address;
   begin
      if Blength > 0 then
         Baddr := B(B'First)'Address;
      end if;
      return Baddr;
   end;
end;
