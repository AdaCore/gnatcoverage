package Pval is
   function F (X : Boolean) return Boolean;
   pragma Precondition (X); -- # eval :o/0:
end;

