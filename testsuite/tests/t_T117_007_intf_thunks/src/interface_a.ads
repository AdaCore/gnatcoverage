pragma Ada_2012;

package Interface_A is

   type T_Interface_A is interface;

   function Get_Variable (This : in T_Interface_A) return Integer is abstract;

   type T_A_Interface_A_Class is access all T_Interface_A'Class;

end Interface_A;
