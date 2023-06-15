with GNAT.IO; use GNAT.IO;

package body Buffer is

   protected body Buffer is

      entry Push (V : Integer; Tell : Boolean)
         when not Has_Value                           -- # push_guard
      is
      begin
         if Tell then                                 -- # push_test_tell
            Put_Line ("push " & Integer'Image (V));   -- # push_tell
         end if;
         Value := V;                                  -- # push_do
         Has_Value := True;                           -- # push_do
      end Push;

      entry Pop (V : out Integer; Tell : Boolean)
         when Has_Value                               -- # pop_guard
      is
      begin
         if Tell then                                 -- # pop_test_tell
            Put_Line ("pop " & Integer'Image (V));    -- # pop_tell
         end if;
         V := Value;                                  -- # pop_do
         Has_Value := False;                          -- # pop_do
      end Pop;

   end Buffer;

end Buffer;
