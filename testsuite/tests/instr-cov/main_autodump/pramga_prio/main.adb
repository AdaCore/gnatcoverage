with System; use System;

procedure Main is
   pragma Priority (Priority'First);

   function Ident (X : Boolean) return Boolean;

   function Ident (X : Boolean) return Boolean is
   begin
      return X;
   end;

   X : Boolean := True;
   pragma Volatile (X);
begin
   X := Ident (X);
end Main;