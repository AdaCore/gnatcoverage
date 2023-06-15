with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package body Synchronization is

   task body Signaler is
   begin
      Buffer := 1;                        -- # prod
      Set_True (Signal_Sent);             -- # signal_send
      Suspend_Until_True (Signal_Ack);    -- # signal_ack_wait
      if Buffer /= 2 then                 -- # test_2
         raise Program_Error;             -- # exception_2
      end if;
   end Signaler;

   task body Waiter is
      Element : Integer;
   begin
      Suspend_Until_True (Signal_Sent);   -- # signal_wait
      if Buffer = 1 then                  -- # test_1
         Buffer := 2;                     -- # change
         Set_True (Signal_Ack);           -- # signal_ack_send
      else
         raise Program_Error;             -- # exception_1
      end if;
   end Waiter;

end Synchronization;
