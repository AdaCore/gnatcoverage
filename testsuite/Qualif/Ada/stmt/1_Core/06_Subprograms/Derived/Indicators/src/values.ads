package Values is
   
   type T_Value is abstract tagged null record;
   
   N_Positives : Integer := 0;
   N_Negatives : Integer := 0;
   N_Zeroes    : Integer := 0;
   
   procedure Characterize1 (V : T_Value) is abstract;
   procedure Characterize2 (V : T_Value) is abstract;
   
   type T_Int is new T_Value with record
      Value : Integer;
   end record;
   
   procedure Characterize1 (IV : T_Int);
   
   overriding
   procedure Characterize2 (IV : T_Int);
   
   procedure T_Int_Characterize1 (IV : T_Int);
   
   not overriding
   procedure T_Int_Characterize2 (IV : T_Int);
     
end;
