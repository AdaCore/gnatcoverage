
package Vbufs is

   type Value is new Integer;

   type Varray is array (Natural range <>) of Value;

   type Vbuffer (Size : Natural) is record
      Store : Varray (1 .. Size); -- store
      Len   : Natural := 0;       -- current # of filled entries
   end record;

   procedure Push (V : Value; VB : in out Vbuffer);

end;

