with System; use System;

with Pkg; use Pkg;

procedure Main is
   pragma Priority (Priority'First);

   function Inner (X : Boolean) return Boolean is separate;

   X : Boolean := True;
   pragma Volatile (X);

   Ctrl_Object : Ctrl_Type;
begin
   X := Inner (X);
end Main;