with Ada.Text_IO; use Ada.Text_IO;

package body Q is
   Ticks, Laps : Integer := 0;

   task Clock is
      entry Start;
      entry Tick;
   end Clock;

   task body Clock is
      Counting : Boolean := False;
   begin
      loop
         select
            accept Start do            -- # start_accept
               Counting := True;       -- # start_stmt
            end;
         or
            accept Tick do             -- # tick_accept
               if Counting then        -- # tick_if
                  Ticks := Ticks + 1;  -- # tick_stmt
               end if;
            end;
         or
            terminate;
         end select;
      end loop;
   end Clock;

   procedure Tryme is
   begin
      Clock.Tick;
      Put_Line (Integer'Image (Ticks));
   end Tryme;

end Q;
