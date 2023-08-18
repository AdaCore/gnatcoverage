package Pkg is

   Custom_Error : Exception;

   function Identity (A : Boolean; Do_Raise : Boolean) return Boolean;
   --  Returns the value of A if Do_Raise is False, raises Custom_Error
   --  otherwise.

end Pkg;
