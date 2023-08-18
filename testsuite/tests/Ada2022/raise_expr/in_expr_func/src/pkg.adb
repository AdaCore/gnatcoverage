pragma Ada_2022;

package body Pkg is

   --  Note: the lone parenthesis is used here to dissociate the statement from
   --  the decision in the coverage reports. It is not a formatting glitch.

   function Identity (A : Boolean; Do_Raise : Boolean) return Boolean is
     (                                                       -- # stmt
      (not Do_Raise or else raise Custom_Error) and then A); -- # expr :o/e:

end Pkg;
