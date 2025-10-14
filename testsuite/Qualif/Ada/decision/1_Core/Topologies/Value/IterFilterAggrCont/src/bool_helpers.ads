--  Helper package to instantiate an ordered set containing booleans. It is not
--  part of the functional code and is thus not subject to coverage analysis.

pragma Ada_2012;

with Ada.Containers.Ordered_Sets;

package Bool_Helpers is

   function "<" (L, R : Boolean) return Boolean is (R or else not L);

   package Bool_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Boolean, "<" => Bool_Helpers."<");
   subtype Bool_Set is Bool_Sets.Set;

end Bool_Helpers;
