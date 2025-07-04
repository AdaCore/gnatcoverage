package Stateful_IO is
   procedure Put (S : String);
   pragma Export (Ada, Put, "stateful_io_put");
end Stateful_IO;
