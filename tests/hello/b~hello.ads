pragma Ada_95;
pragma Restrictions (No_Exception_Propagation);
with System;
package ada_main is
   pragma Warnings (Off);


   GNAT_Version : constant String :=
                    "GNAT Version: Pro 6.1.1 (20080122-41)";
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_hello" & Ascii.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure Break_Start;
   pragma Export (C, Break_Start, "__gnat_break_start");

   procedure main;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#1d25d8e7#;
   u00002 : constant Version_32 := 16#279dc047#;
   u00003 : constant Version_32 := 16#d42f984b#;

   pragma Export (C, u00001, "helloB");
   pragma Export (C, u00002, "textioB");
   pragma Export (C, u00003, "textioS");

   --  BEGIN ELABORATION ORDER
   --  textio%s
   --  textio%b
   --  hello%b
   --  END ELABORATION ORDER

end ada_main;
