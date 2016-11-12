with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package FUAND is
   
   type Key is new String (1 .. 5);
   
   type Caller_Operand is record
      X : Integer;
      US : Unbounded_String;
   end record;
   
   type Operands is record
      A, B : Caller_Operand;
   end record;
   
   type Operand is record
      Pos : Boolean;
      K : Unbounded_String;
   end record;
     
   TTA : constant Caller_Operand := (1, To_Unbounded_String ("USA"));
   TTB : constant Caller_Operand := (5, To_Unbounded_String ("USB"));
   
   FFA : constant Caller_Operand := (-1, To_Unbounded_String ("USA"));
   FFB : constant Caller_Operand := (1, To_Unbounded_String ("not USB"));
   
   function Andthen (Ops : Operands) return Boolean;
end;
