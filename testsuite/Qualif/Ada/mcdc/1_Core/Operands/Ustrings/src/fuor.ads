with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package FUOR is
   
   type Key is new String (1 .. 5);
   
   type Caller_Operand is record
      US1, US2 : Unbounded_String;
   end record;
   
   type Operands is record
      A, B : Caller_Operand;
   end record;
   
   type OperandA is record
      Match : Boolean;
   end record;
   
   type OperandB is record
      To_Match : Unbounded_String;
   end record;
     
   TTA : constant Caller_Operand := 
     (To_Unbounded_String ("START"), To_Unbounded_String("END"));
   TTB : constant Caller_Operand := 
     (To_Unbounded_String("CAB"), To_Unbounded_String("201"));
   
   FFA : constant Caller_Operand := 
     (To_Unbounded_String("A"), To_Unbounded_String("B"));
   FFB : constant Caller_Operand :=
     (To_Unbounded_String("CAB"), To_Unbounded_String("11"));
   
   function Orelse (Ops : Operands) return Boolean;
end;
