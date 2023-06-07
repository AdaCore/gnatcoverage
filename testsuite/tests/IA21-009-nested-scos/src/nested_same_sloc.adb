pragma Warnings (Off);
procedure Nested_Same_Sloc is
   A, B : Boolean;
begin
   if (A and then B) = True then
      null;
   end if;
   raise Program_Error;
end Nested_Same_Sloc;
