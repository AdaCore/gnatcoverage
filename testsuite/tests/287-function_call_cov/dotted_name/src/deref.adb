pragma Ada_2012;

--  Dotted_Name in the context of an explicit dereference

procedure Deref is

   type String_Access is access String;                          -- # ok

   S : String_Access := new String'("Hello World!");             -- # ok

   Function Id (S : String_Access) return String_Access is (S);  -- # ok

   Dummy : String := Id (S).all;                                 -- # deref1
   F : Boolean := False;                                         -- # ok
begin
   if F then                                                     -- # if
      Dummy := Id (S).all;                                       -- # deref2
   end if;
end Deref;
