generic
   type Value is private;

package Gstacks is

   type Stack (Size : Natural) is private;

   procedure Push (V : Value; S : in out Stack);
   procedure Pop (V : out Value; S : in out Stack);

   function N_Values (S : in Stack) return Natural;

private
   type Value_Array is array (Natural range <>) of Value;
   type Stack (Size : Natural) is record
      Store : Value_Array (1 .. Size);
      N_Values : Natural := 0;
   end record;
end;
