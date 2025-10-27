pragma Ada_2012;

package Pkg is

   type Item_Type is (First, Last);

   type Composite is private;

   Cached_Comp : constant Composite;

   function Make (First, Last : Integer) return Composite;

   function Eq (Comp : Composite; First, Last : Integer) return Boolean;

   function Copy_With_Abs_Update
     (Input     : Composite;
      Item      : Item_Type;
      Use_Cache : Boolean := False) return Composite;
   --  Return a copy of Input or Cached_Comp depending on the value of
   --  Use_Cache, with either the "First" or "Last" component being updated to
   --  its absolute value.
private
   type Composite is record
      First : Integer;
      Last  : Integer;
   end record;

   Cached_Comp : constant Composite := (0, 0);

   ----------
   -- Make --
   ----------

   function Make (First, Last : Integer) return Composite is ((First, Last));

   --------
   -- Eq --
   --------

   function Eq (Comp : Composite; First, Last : Integer) return Boolean is
     (Comp.First = First and then Comp.Last = Last);

end Pkg;
