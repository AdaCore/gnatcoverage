with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Who : constant String := "world";
begin
   if Who'Length = 0 then
      raise Constraint_Error;
   elsif Who'Length = 1 and Who (Who'First) = 'h' then
      raise Constraint_Error;
   elsif Who'Length = 0 or Who (Who'First) = 'h' then
      raise Constraint_Error;
   elsif Who'Length = 0 xor Who (Who'First) = 'h' then
      raise Constraint_Error;
   elsif Who'Length = 0 or else Who (Who'First) = 'h' then
      raise Constraint_Error;
   else
      Put_Line ("Hello " & Who);
   end if;
end Main;
