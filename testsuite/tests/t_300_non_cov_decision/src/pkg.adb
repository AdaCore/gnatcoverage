pragma Ada_2012;

with Ada.Text_IO;

package body Pkg is

   procedure Foo (X : Boolean) is
      type String_Acc is access all String;
      Str     : aliased String := "Hello World!";   -- # st
      Str_Acc : constant String_Acc := Str'Access;  -- # st
      Cond    : Boolean := True;                    -- # st
      pragma Volatile(Cond);
      Ptr     : String_Acc := (if Cond then Str_Acc else Str_Acc);  -- # dc
   begin
      Ada.Text_IO.Put_Line (Ptr.all);  -- # st
   end Foo;

end Pkg;
