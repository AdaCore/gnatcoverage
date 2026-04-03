with Ada.Text_IO;
with Interfaces.C;

procedure Main is
   Fixed_Bool : Boolean := False;
   pragma Volatile (Fixed_Bool);
   function Main_C return Interfaces.C.Int;
   pragma Import (C, Main_C, External_Name => "main_c");
   Dummy_Int : constant Interfaces.C.Int := Main_C;
begin
   if Fixed_Bool then
      raise Program_Error;
   else
      Ada.Text_IO.Put_Line ("Yay!");
   end if;
   pragma Annotate (Xcov, Exempt_On);
   if Fixed_Bool then
      raise Program_Error;
   end if;
   pragma Annotate (Xcov, Exempt_Off);
end Main;
