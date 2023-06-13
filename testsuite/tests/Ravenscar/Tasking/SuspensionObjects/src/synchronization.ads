--  Test to check whether we can analyze correctly parts of an execution
--  which are covered after a task is blocked in a suspension object.

with System;
with Ada.Synchronous_Task_Control;

package Synchronization is

   task Signaler is
      pragma Priority (System.Priority'Last - 1);
   end Signaler;

   task Waiter is
      pragma Priority (System.Priority'Last);
   end Waiter;

   Signal_Sent, Signal_Ack : Ada.Synchronous_Task_Control.Suspension_Object;
   Buffer : Integer := 0;
   pragma Volatile (Buffer);

end Synchronization;
