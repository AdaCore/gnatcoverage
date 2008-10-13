pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~hello.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~hello.adb");

package body ada_main is
   pragma Warnings (Off);
   procedure adainit is
      E3 : Boolean; pragma Import (Ada, E3, "textio_E");

   begin
      null;

      E3 := True;
   end adainit;

   procedure adafinal is
   begin
      null;
   end adafinal;

   procedure Break_Start is
   begin
      null;
   end;

   procedure main is

      procedure Ada_Main_Program;
      pragma Import (Ada, Ada_Main_Program, "_ada_hello");

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      adainit;
      Break_Start;
      Ada_Main_Program;
   end;

--  BEGIN Object file/option list
   --   .\textio.o
   --   .\hello.o
   --   -L.\
   --   -L../support/src/ppc-elf\
   --   -LC:\Programs\AdaTEST95\2.0\lib\adatest\
   --   -LC:\GNATPRO\6.1.1\tools\asis\asis-6.1.1-src\obj\
   --   -LC:\GNATPRO\6.1.1\tools\asis\asis-6.1.1-src\lib\
   --   -LC:\GNATPRO\6.1.1\tools\asis\asis-6.1.1-src\asis\
   --   -LC:\GNATPRO\6.1.1\tools\asis\asis-6.1.1-src\gnat\
   --   -LC:/GNATPRO/6.1.1/lib/gcc/powerpc-elf/4.1.3/rts-zfp/adalib\
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
