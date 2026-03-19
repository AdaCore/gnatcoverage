package Vars is
   X : Integer := 12;
   pragma Volatile (X);

   procedure Assert_Eq (Value, Expected : Integer);
end;
