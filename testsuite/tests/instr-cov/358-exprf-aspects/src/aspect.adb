pragma Ada_2012;

procedure Aspect is

   function Foo return Integer is (42)                     -- # expr
     with Convention => Stdcall;

   type Foo_Type is access
      function return Integer with Convention => Stdcall;  -- # type

   Dummy : Foo_Type := Foo'Access;                         -- # dummy
begin
   null;
end Aspect;
