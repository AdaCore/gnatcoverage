package FUOR is
   
   type Int is new Integer range 600 .. 607;
   
   type Char is new Character range 'A' .. 'Z';
   type Char_Array is array (Int'Range) of Char;
   
   type Ref is record
      Index : Int;
      Data : Char_Array;
   end record;
   
   type Operands is record
      A, B : Ref;
   end record;
   
   -- We will evaluate for each operand whether .data(.index) = 'T'
   
   Common_Data : constant Char_Array := 
     (600 => 'A', 
      601 => 'B',
      602 => 'T',
      603 => 'Y',
      604 => 'Z',
      605 => 'T',
      606 => 'U',
      607 => 'V');
   
   TT1 : constant Ref := (Index => 602, Data => Common_Data);
   TT2 : constant Ref := (Index => 605, Data => Common_Data);
   FF1 : constant Ref := (Index => 600, Data => Common_Data);
   FF2 : constant Ref := (Index => 607, Data => Common_Data);
				
   function Orelse (Ops : Operands) return Boolean;
end;
