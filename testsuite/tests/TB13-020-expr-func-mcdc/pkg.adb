package body Pkg is

   -------------
   -- Is_Null --
   -------------

   function Is_Null (I : Integer) return Boolean is
   begin
      return I = 0;
   end Is_Null;

end Pkg;
