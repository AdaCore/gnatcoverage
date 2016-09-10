
-- Case where the object types are derived from a single Interface type.

package I_Objects is
   
   --  The base Interface type:
   
   type I_Value is Interface;
   
   N_Positives : Integer := 0;
   N_Negatives : Integer := 0;
   N_Zeroes    : Integer := 0;
   
   procedure Characterize (V : I_Value) is abstract;
   
   --
   
   type T_Int is new I_Value with record
     Value : Integer;
   end record;
   
   overriding
   procedure Characterize (IV : T_Int);
   
   --
   
   type T_Float is new I_Value with record
     Value : Float;
   end record;
   
   overriding
   procedure Characterize (FV : T_Float);
   
end;
