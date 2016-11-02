package FUOR is
   
   type Key is new String (1 .. 5);
   
   type Caller_Operand is record
      X, Y : Integer;
      K : Key;
   end record;
   
   type Operands is record
      A, B : Caller_Operand;
   end record;
   
   type Operand is record
      Pos, Even : Boolean;
      K : Key;
   end record;
     
   TTA : constant Caller_Operand := (1, 6, "AAKEY");
   TTB : constant Caller_Operand := (5, 5, "BBKEY");
   
   FFA : constant Caller_Operand := (1, 6, "AAKEX");
   FFB : constant Caller_Operand := (1, 6, "BBKEY");
   
   function Orelse (Ops : Operands) return Boolean;
end;
