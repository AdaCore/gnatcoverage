pragma Ada_2012;

package body Pkg.Simple is

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Simple_Object) return Boolean is
   begin
      return False;
   end Is_Null;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model (Self : Simple_Object) return Model'Class is
      Result : constant Model := (Not_Null => True);
   begin
      return Result;
   end Get_Model;

end Pkg.Simple;
