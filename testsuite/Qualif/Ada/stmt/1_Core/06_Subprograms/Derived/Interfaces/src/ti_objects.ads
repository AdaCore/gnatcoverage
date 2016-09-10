
-- Case where the object types are the combination of a Tagged
-- type and an Interface type.

package TI_Objects is
   
   -- The Tagged type:
   
   type Identifiable is abstract tagged record
      Id : Integer := -1;
   end record;
   
   procedure Assign_Id_To (O : in out Identifiable);
   
   --  The base Interface type:
   
   type I_Value is Interface;
   
   N_Positives : Integer := 0;
   N_Negatives : Integer := 0;
   N_Zeroes    : Integer := 0;
   
   procedure Characterize (V : I_Value) is abstract;
   
   --
   
   type T_Int is new Identifiable and I_Value with record
     Value : Integer;
   end record;
   
   overriding
   procedure Characterize (IV : T_Int);
   
   --
   
   type T_Float is new Identifiable and I_Value with record
     Value : Float;
   end record;
   
   overriding
   procedure Characterize (FV : T_Float);
   
end;
