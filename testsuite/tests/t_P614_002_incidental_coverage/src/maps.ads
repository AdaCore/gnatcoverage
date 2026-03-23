package Maps is
   type Rectangle_T is record
      L, W : Natural;
   end record;

   function Area (R : Rectangle_T) return Natural;
end;
