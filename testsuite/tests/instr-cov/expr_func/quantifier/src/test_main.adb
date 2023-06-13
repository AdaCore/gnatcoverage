pragma Ada_2012;

procedure Test_Main is

   type Int_Array is array (Positive range <>) of Integer;

   function All_Multiple_Of_2_And_3 (IA : Int_Array) return Boolean is
     (for all I in IA'Range =>                      -- # func-stmt
      IA (I) mod 2 = 0 and then IA (I) mod 3 = 0);  -- # func-dc

begin
   if All_Multiple_Of_2_And_3 ((2, 4, 12)) then -- # call-dc
      raise Program_Error;           -- # call-error
   end if;
end Test_Main;

--# test_main.adb
--
-- /func-stmt/  l+ ## 0
-- /func-dc/    l! ## dT-
-- /call-dc/    l! ## dT-
-- /call-error/ l- ## s-
