with Update_G;
with Stacks_G;
with Pack;
package Local_Instantiations is
   package Stacks is new Stacks_G (Integer, 1);

   procedure Update is new Update_G (Integer);

   function New_Value is new Pack.New_Value_G (Integer);

   package Pack_Instance is new Pack.Pack_G (Character, 'A');

   procedure Proc_With_Instantiations
     (I, J     : in out Integer;
      B1, B2 : in out Boolean);
   --  Contains instantiations in the body;

   function Fun_With_Instantiations (I : Integer) return Integer;
   --  Contains instantiations in the body;
end  Local_Instantiations;
