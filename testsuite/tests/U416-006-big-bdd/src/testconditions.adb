with Ada.Text_IO;

package body TestConditions is

   C1 : Boolean := False;
   C2 : Boolean := False;
   C3 : Boolean := True;
   C4 : Boolean := False;
   C5 : Boolean := True;
   C6 : Boolean := False;
   C7 : Boolean := False;
   C8 : Boolean := True;

   function Is_Activated return Boolean is
   begin
      return
        ((((((((((((((( C1 or else C2 ) and then C3 ) or else (( C1 or else C2 ) and then C3 )) or else ((( C4 or else C1 ) or else C2 ) and then C3 )) or else ((( C4 or else C1 ) or else C2 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else (( C7 or else C1 ) and then C3 )) or else (( C7 or else C1 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else (( C1 or else C7 ) and then C3 )) or else ((((( C5 or else C6 ) or else C4 ) or else C1 ) or else C7 ) and then C3 )) or else (( C5 or else C8 ) and then C3 );
   end Is_Activated;

   procedure Run is
   begin
      if Is_Activated then
         Ada.Text_IO.Put_Line ("Is Activated");
         C3 := False;
         C5 := False;
         C8 := False;
         Run;
      end if;
   end Run;

end TestConditions;
