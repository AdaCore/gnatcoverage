package Optypes is
   type T_Base_Operand is tagged record
     Valid : Boolean := False;
     Value : Integer;
   end record;
   
   type T_Operation is (Op_Add, Op_Sub);
end;
