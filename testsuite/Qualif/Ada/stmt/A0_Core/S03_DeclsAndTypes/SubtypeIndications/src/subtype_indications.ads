--  This package contains subtype indications in various contexts.

with Decls_Support; use Decls_Support;
with Support;       use Support;
package Subtype_Indications is

   type Arr is array (1 .. 5) of Integer range 1 .. Identity (2);     -- # dcl

   function Get_Var_String
     (Len : Index;
      Data : String)
      return Var_String;
   --  Creates from its arguments the corresponding Var_String value. The body
   --  contains subtype indication in subtype declaration

   function Simple_Sort (V : Vector) return Vector;
   --  Sorts ints argument. The body contains subtype indication in object
   --  declaration

   function Some_Fun
     (Len : Integer;
      S   : String)
      return Integer;
   --  Is completely meaningless. The main goal is to use a subtype indication
   --  in component definition
end Subtype_Indications;
