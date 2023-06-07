with System; use System;

package body Services is
   procedure Validate (T : Boolean) is
   begin
      if not T then
         raise Program_Error;
      end if;
   end;

   procedure Check_Value_And (This : in out Str_Type) is
   begin
      Validate (This.Size = 0 or else This.Value (1) = 'X');
   end;

   procedure Check_Value_Or (This : in out Str_Type) is
   begin
      Validate (This.Size = 0 or else This.Value (1) = 'X');
   end;

   procedure Check_Addr_And (This : Str_Type) is
   begin
      Validate (This'Address /= System.Null_Address
                  and then This.Value'Address /= System.Null_Address);
   end;

   procedure Check_Addr_Or (This : Str_Type) is
   begin
      Validate (This'Address /= System.Null_Address
                  or else This.Value'Address /= System.Null_Address);
   end;

end;
