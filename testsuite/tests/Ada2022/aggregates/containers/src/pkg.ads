with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

package Pkg is

   package Int_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Integer, Element_Type => Integer);
   subtype Int_Map is Int_Maps.Map;

   package Int_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);
   subtype Int_Set is Int_Sets.Set;

   function Overly_Complex_Identity_Build (Input : Int_Set) return Int_Map;
   --  Build an identity map for the elements of Input

end Pkg;
