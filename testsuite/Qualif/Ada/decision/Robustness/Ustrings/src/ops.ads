with Ada.Strings.Unbounded;
use Ada.Strings; use Ada.Strings.Unbounded;

package Ops is
   procedure Tryme (A : String; N : Natural; B : String; X : Integer);
   
   Last_Long, Last_Short, Last_mapped : Unbounded_String := Null_Unbounded_String;
   N_Xpos, N_Xneg, N_Xzero : Integer := 0;
   
   type Last_Category is (Long, Short, Mapped);
   
   procedure Check (Which : Last_Category; S : String);
end;
