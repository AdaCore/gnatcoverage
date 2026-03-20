with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Strict_Head
  (S : Unbounded_String; Len : Positive) return Unbounded_String
is
begin
   if Length (S) < Len then
      raise Constraint_Error;
   end if;
   return Head (S, Len);
end Strict_Head;
