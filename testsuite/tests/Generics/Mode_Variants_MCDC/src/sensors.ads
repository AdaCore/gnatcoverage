with Check_G;

package Sensors is
   type Temp_C is new Float range -273.15 .. Float'Last;
   package RC is new Check_G (Temp_C, 0.0, 100.0); -- # i:RC

   type Temp_F is new Float range -459.67 .. Float'Last;
   package RF is new Check_G (Temp_F, 32.0, 212.0);-- # i:RF

   type Temp_Unit is (C, F);

   procedure Test (Value : Float; Unit : Temp_Unit);
   
   N_Tests_C, N_Tests_F : Natural := 0;
end Sensors;
