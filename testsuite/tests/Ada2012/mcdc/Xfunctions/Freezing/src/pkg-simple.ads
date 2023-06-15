pragma Ada_2012;

package Pkg.Simple is
   type Simple_Object is new Object with private;
   Singleton : constant Simple_Object;
   function Is_Null (Self : Simple_Object) return Boolean;
   function Get_Model (Self : Simple_Object) return Model'Class;
private
   type Simple_Object is new Object with null record;
   Singleton : constant Simple_Object := (null record);
end Pkg.Simple;
