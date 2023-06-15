--  Test to check whether we can analyze correctly parts of an execution
--  which are covered after a task is blocked.

with System;

package Synchronization is

   task Producer is
      pragma Priority (System.Priority'Last - 1);
   end Producer;

   task Consumer is
      pragma Priority (System.Priority'Last);
   end Consumer;

   protected Buffer is
      procedure Put (Item : Integer);
      entry Get (Item : out Integer);
   private
      Element : Integer := 0;
      Barrier : Boolean := False;
   end Buffer;

end Synchronization;
