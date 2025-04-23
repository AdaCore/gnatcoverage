package Andthen is
   subtype Custom is Boolean;
   function Make_Custom (X : Boolean) return Custom;
   function And_Then_Custom (A, B : Custom) return Custom;
end;
