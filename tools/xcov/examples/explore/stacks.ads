---------------------------------------------------------------------------
--                              STACKS (SPEC)                            --
---------------------------------------------------------------------------

--  This package offers a simple "bounded stack" data structure abstraction

generic
   type Data_Type is private;  --  The elements data type
package Stacks is
   type Stack (Capacity : Natural) is private;

   function Full (S : Stack) return Boolean;
   --  Whether S is full with respect to its Capacity.

   function Empty (S : Stack) return Boolean;
   --  Whether S is empty.

   procedure Push (Item : Data_Type; S : in out Stack);
   --  Push Item on top of stack S if it is not Full,
   --  raise Program_Error and leave S unchanged otherwise.

   procedure Pop (Item : out Data_Type; S : in out Stack);
   --  Pop the top element off stack S into Item if S is not Empty,
   --  raise Program_Error and leave Item undefined otherwise.

private
   type Data_Array is array (Natural range <>) of Data_Type;
   type Stack (Capacity : Natural) is record
      Items : Data_Array (1 .. Capacity);
      Top_In : Natural := 0;
      --  The index in Items where the top element is stored.
      --  Increase on successful push, decrease on successful pop.
   end record;

end;
