package FUAND is
   
   type Int is new Integer range 600 .. 607;
   
   type Char is new Character range 'A' .. 'Z';
   type Char_Array is array (Int'Range) of Char;
   
   type Operand is record
      Index : Int;
      Data : Char_Array;
   end record;
   
   type Operands is record
      A, B : Operand;
   end record;
   
   -- We will evaluate for each operand whether .data(.index+1) = 'T'
   
   Common_Data : constant Char_Array := 
     (600 => 'A', 
      601 => 'B',
      602 => 'T',
      603 => 'Y',
      604 => 'Z',
      605 => 'T',
      606 => 'U',
      607 => 'V');
   
   TT1 : constant Operand := (Index => 601, Data => Common_Data);
   TT2 : constant Operand := (Index => 604, Data => Common_Data);
   FF1 : constant Operand := (Index => 600, Data => Common_Data);
   FF2 : constant Operand := (Index => 605, Data => Common_Data);
      
   function Andthen (Ops : Operands) return Boolean;
end;
