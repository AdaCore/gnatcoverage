pragma Ada_2012;

package Pkg is

   type Model is tagged private;
   type Object is interface;

   function Is_Null (Self : Object) return Boolean is abstract;
   function Clone (Self : Object'Class) return Object'Class with
      Post => not Clone'Result.Get_Model.Is_Null;
   function Get_Model (Self : Object) return Model'Class is abstract;
   function Get_Id (Self : Object'Class) return Natural is
     (if Self.Is_Null or else Self.Clone.Is_Null -- # get-id-cond
      then 0                                     -- # get-id-then
      else 1);                                   -- # get-id-else

   function Is_Null (Self : Model'Class) return Boolean;

private
   type Model is tagged record
      Not_Null : Boolean;
   end record;
end Pkg;
