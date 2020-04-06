package Par.Andthen is
   function False return Custom renames Par.False;
   function True return Custom renames Par.True;
   function "=" (A, B : Custom) return Boolean renames Par."=";
   function Make_Custom (X : Boolean) return Custom;
   function And_Then_Custom (A, B : Custom) return Custom;
end;
