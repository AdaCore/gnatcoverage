pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type String_Acc is access all String;
   Str     : aliased String := "Hello World!";
   Str_Acc : constant String_Acc := Str'Access;
   Cond    : Boolean := True;
   pragma Volatile(Cond);
   Ptr     : String_Acc := (if Cond then Str_Acc else Str_Acc);
begin
   Put_Line ("Yay!");
   if Cond then
      Ptr := Str_Acc;
   else
      Ptr := Str_Acc;
   end if;
   Put_Line (Ptr.all);
end Main;
