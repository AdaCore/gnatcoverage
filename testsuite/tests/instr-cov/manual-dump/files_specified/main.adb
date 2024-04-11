with Foo;
with Foo_Skipped;

procedure Main is
   procedure Foo_C;
   pragma Import (C, Foo_C, "foo_c");

   procedure Foo_Skipped_C;
   pragma Import (C, Foo_Skipped_C, "foo_skipped_c");

   procedure Foo_Cpp;
   pragma Import (CPP, Foo_Cpp, "foo_cpp");

   procedure Foo_Skipped_Cpp;
   pragma Import (CPP, Foo_Skipped_CPP, "foo_skipped_cpp");

begin
   Foo;
   pragma Annotate (Xcov, Reset_Buffers);
   Foo_Skipped;
   pragma Annotate (Xcov, Reset_Buffers);
   Foo_C;
   pragma Annotate (Xcov, Reset_Buffers);
   Foo_Skipped_C;
   pragma Annotate (Xcov, Reset_Buffers);
   Foo_Cpp;
   pragma Annotate (Xcov, Reset_Buffers);
   Foo_Skipped_Cpp;
   pragma Annotate (Xcov, Reset_Buffers);
end Main;
