with Interfaces.C; use Interfaces.C;

package Pkg is
   function And_Then (A, B : int) return int;
   pragma Import
     (Convention => C, Entity => And_Then, External_Name => "and_then");

   function Id (A : Boolean) return Boolean;
end Pkg;
