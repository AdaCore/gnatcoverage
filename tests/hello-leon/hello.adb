with Uart; use Uart;

procedure Hello is
   Res : Character;
begin
   loop
      Put_Line ("Menu:");
      Put_Line ("1) Hello");
      Put_Line ("2) Bye");
      Put_Line ("3) Quit");
      Put ("You choice: ");
      Get (Res);
      New_Line;
      Put (Res);
      New_Line;
      case Res is
         when '1' =>
            Put_Line ("hello");
         when '2' =>
            Put_Line ("bye");
         when '3' =>
            return;
         when ASCII.CR | ASCII.LF =>
            null;
         when others =>
            Put_Line ("invalid choice");
      end case;
   end loop;
end Hello;
