--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pkg2.Test_Data.

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
package body Pkg2.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add0 (Gnattest_T : in out Test);
   procedure Test_Add0_4012d3 (Gnattest_T : in out Test) renames Test_Add0;
--  id:2.2/4012d3b0bc08ba75/Add0/1/0/
   procedure Test_Add0 (Gnattest_T : in out Test) is
   --  pkg2.ads:10:4:Add0
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
         (Add0(0) = 0, "Add0 returned wrong value");

--  begin read only
   end Test_Add0;
--  end read only


--  begin read only
   procedure Test_Add3 (Gnattest_T : in out Test);
   procedure Test_Add3_2f3175 (Gnattest_T : in out Test) renames Test_Add3;
--  id:2.2/2f3175843c7388b8/Add3/1/0/
   procedure Test_Add3 (Gnattest_T : in out Test) is
   --  pkg2.ads:11:4:Add3
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
         (Add3(0) = 3, "Add1 returned wrong value");


--  begin read only
   end Test_Add3;
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
end Pkg2.Test_Data.Tests;
