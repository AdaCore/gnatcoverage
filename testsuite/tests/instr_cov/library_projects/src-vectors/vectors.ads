generic
   type Element_Type is private;
   Capacity : Natural;
package Vectors is

   type Vector is private;

   function Last_Index (Self : Vector) return Natural;
   function Element (Self : Vector; Index : Positive) return Element_Type;
   procedure Push (Self : in out Vector; Element : Element_Type);
   procedure Pop (Self : in out Vector);

private

   type Element_Array is array (1 .. Capacity) of Element_Type;

   type Vector is record
      Elements   : Element_Array;
      Last_Index : Natural := 0;
   end record;

end Vectors;
