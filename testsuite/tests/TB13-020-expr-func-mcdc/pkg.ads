package Pkg is
   Flag : Boolean := True;
   type Point is record
      X, Y : Integer;
   end record;

   function Is_Null (I : Integer) return Boolean;
end Pkg;
