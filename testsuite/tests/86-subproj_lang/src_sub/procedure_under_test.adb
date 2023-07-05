pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;

package body Procedure_Under_Test is

   procedure Test
     (Control : Integer;
      In_A    : Integer;
      In_B    : Integer)
   is
      Result : Integer := 0;
   begin

      if Control < 0 then
         Result := Calculations_Problematic (In_A, In_B);
      else
         Result := Calculations (In_A, In_B);
      end if;

      Put_Line (Result'Image);

   end Test;

end Procedure_Under_Test;
