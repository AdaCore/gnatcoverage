---------------------------------------------------------------------------
--                              QUEUES (SPEC)                            --
---------------------------------------------------------------------------

--  This package offers a simple "bounded queue" data structure abstraction

generic
   type Data_Type is private;  --  The elements data type
package Queues is
   type Queue (Capacity : Natural) is private;

   function Full (Q : Queue) return Boolean;
   --  Whether S is full with respect to its Capacity.

   function Empty (Q : Queue) return Boolean;
   --  Whether S is empty.

   procedure Push (Item : Data_Type; Q : in out Queue);
   --  Push Item at the back of queue Q if it is not Full,
   --  raise Program_Error and leave S unchanged otherwise.

   procedure Pop (Item : out Data_Type; Q : in out Queue);
   --  Pop the top element off the head of Q into Item if Q is not Empty,
   --  raise Program_Error and leave Item undefined otherwise.

private
   type Data_Array is array (Natural range <>) of Data_Type;
   type Queue (Capacity : Natural) is record
      Items : Data_Array (1 .. Capacity);
      Front, Back : Natural := 1;
      Size : Natural := 0;
   end record;
end;
