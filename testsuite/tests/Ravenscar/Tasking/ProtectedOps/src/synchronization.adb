package body Synchronization is

   task body Producer is
   begin
      Buffer.Put (1);         -- # prod
   end Producer;

   task body Consumer is
      Element : Integer;
   begin
      Buffer.Get (Element);   -- # cons
      if Element /= 1 then    -- # test
	 raise Program_Error; -- # exception
      end if;
   end Consumer;

   protected body Buffer is
      procedure Put (Item : Integer) is
      begin
         Element := Item;     -- # put_element
         Barrier := True;     -- # open_barrier
      end Put;

      entry Get (Item : out Integer) when Barrier is
      begin
         Item := Element;     -- # get_element
         Barrier := False;    -- # close_barrier
      end Get;
   end Buffer;

end Synchronization;
