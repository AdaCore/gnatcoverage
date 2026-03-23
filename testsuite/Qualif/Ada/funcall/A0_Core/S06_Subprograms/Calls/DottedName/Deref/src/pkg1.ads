package Pkg1 is

   type Pkg1_String_Access is access String;

   type T is record
      Field : Pkg1_String_Access := new String'("Hello World!");
   end record;
end Pkg1;
