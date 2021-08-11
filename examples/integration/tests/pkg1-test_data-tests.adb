--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pkg1.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Pkg1.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add1 (Gnattest_T : in out Test);
   procedure Test_Add1_b12835 (Gnattest_T : in out Test) renames Test_Add1;
--  id:2.2/b1283521e6a5072c/Add1/1/0/
   procedure Test_Add1 (Gnattest_T : in out Test) is
   --  pkg1.ads:10:4:Add1
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Add1;
--  end read only


--  begin read only
   procedure Test_Add2 (Gnattest_T : in out Test);
   procedure Test_Add2_2c2b25 (Gnattest_T : in out Test) renames Test_Add2;
--  id:2.2/2c2b258c75afa42d/Add2/1/0/
   procedure Test_Add2 (Gnattest_T : in out Test) is
   --  pkg1.ads:11:4:Add2
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Add2;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Pkg1.Test_Data.Tests;
