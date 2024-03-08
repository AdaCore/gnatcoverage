package Utils is

   pragma No_Elaboration_Code_All;
   pragma Preelaborate;

   function Identity (I : Integer) return Integer;
   --  The only purpose of this function is to break the compiler's data flow
   --  analysis so that high optimization levels cannot optimize code too much.

end Utils;
