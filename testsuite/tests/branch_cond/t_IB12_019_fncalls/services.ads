package Services is
   type Int is record
      Value : Integer;
   end record;

   function GT (X, Y : Int) return Boolean;
   function EQ (X, Y : Int) return Boolean;
   function GE (X, Y : Int) return Boolean;
end;
