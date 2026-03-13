with Ada.Text_IO;

procedure Main is
   Fixed_Bool : Boolean := False;
   pragma Volatile (Fixed_Bool);
begin
   if Fixed_Bool then
      raise Program_Error with "Unreachable";
   else
      Ada.Text_IO.Put_Line ("Yay!");
   end if;
   pragma Annotate (Xcov, Exempt_On);
   if Fixed_Bool then
      raise Program_Error with "Doubly unreachable";
   end if;
   pragma Annotate (Xcov, Exempt_Off);
end Main;
