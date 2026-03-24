pragma Ada_2012;

with Pkg2;

--  Calls in the context of an explicit dereference

procedure Deref is

   type String_Access is access String;                          -- # ok

   S : String_Access := new String'("Hello World!");             -- # ok

   Function Id (S : String_Access) return String_Access is (S);  -- # ok

   --  Explicit dereference with visible return type
   Dummy : String := Id (S).all;                                 -- # deref1

   F : Boolean := False;                                         -- # ok

begin
   --  Explicit dereference with invisible return type
   Dummy := Pkg2.F.Field.all;                                    -- # deref2

   if F then                                                     -- # if
      --  Explicit dereference with visible return type
      Dummy := Id (S).all;                                       -- # deref3

      --  Explicit dereference with invisible return type
      Dummy := Pkg2.F.Field.all;                                 -- # deref4
   end if;
end Deref;
