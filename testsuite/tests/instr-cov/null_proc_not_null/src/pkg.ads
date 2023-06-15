pragma Ada_2012;

package Pkg is

   type String_Access is access all String;
   My_String : aliased String := "hello world";

   procedure Ignore_1 (S : not null String_Access) is null;          -- # p-1
   procedure Ignore_2 (S : not null access constant String) is null; -- # p-2

end Pkg;
