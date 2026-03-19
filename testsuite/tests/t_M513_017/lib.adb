package body Lib is

   function Image (I : Integer) return String is
   begin
      if I < 0 then
         return '-' & Integer'Image (I);
      else
         return Integer'Image (I);
      end if;
   end Image;

   function Get (I : Integer) return Boolean is
   begin
      if Image (I) = Image (1) & "0" then
         return True;
      else
         return False;
      end if;
   end Get;

   function Get2 (I : Integer) return Boolean is
   begin
      return Image (I) /= "10" and then Image (I) /= "20";
   end Get2;
end Lib;
