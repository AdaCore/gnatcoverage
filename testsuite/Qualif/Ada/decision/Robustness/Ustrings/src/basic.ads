with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Basic is
   
   Last_Long, Last_Short : Unbounded_String := Null_Unbounded_String;
   
   procedure Try1 (A : String);
   procedure Try2 (A : String);
   procedure Try3 (A : String);
end;

