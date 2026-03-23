package body Vectors is

   function Last_Index (Self : Vector) return Natural is
   begin
      return Self.Last_Index;
   end Last_Index;

   function Element (Self : Vector; Index : Positive) return Element_Type is
   begin
      return Self.Elements (Index);
   end Element;

   procedure Push (Self : in out Vector; Element : Element_Type) is
   begin
      if Self.Last_Index = Capacity then
         raise Constraint_Error;
      else
         Self.Last_Index := Self.Last_Index + 1;
         Self.Elements (Self.Last_Index) := ElemenT;
      end if;
   end Push;

   procedure Pop (Self : in out Vector) is
   begin
      if Self.Last_Index = 0 then
         raise Constraint_Error;
      else
         Self.Last_Index := Self.Last_Index - 1;
      end if;
   end Pop;

end Vectors;
