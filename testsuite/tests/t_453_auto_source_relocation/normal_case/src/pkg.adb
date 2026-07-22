with System;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Pkg is
   procedure Foo (Name : String) is
      procedure Foo_C (Addr : chars_ptr; Length : unsigned);
      pragma
        Import (Convention => C, Entity => Foo_C, External_Name => "foo_c");

      Name_C : chars_ptr := New_String (Name);
   begin
      Foo_C (Name_C, Name'Length);
      Free (Name_C);
   end Foo;
end Pkg;
